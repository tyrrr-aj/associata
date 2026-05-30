-module(agds).
-export([create/1, end_experiment/1]).
-export([notify_node_stimulated/2]).    % internal

-include("config.hrl").
-include("stimulation.hrl").

-record(node_groups, {
    vngs = #{},
    ong
}).

-record(transient_state, {
    last_stimulation_id=none
}).

-record(configuration, {
    global_cfg
}).

-record(profiling_info, {
    inference_time_native=0
}).

-record(logging_info, {
    log_file
}).

-record(state, {
    structure_id,
    node_groups = #node_groups{},
    transient_state = #transient_state{},
    configuration,
    profiling_info = #profiling_info{},
    logging_info
}).


%% %%%%%%%%%%%%%%% Public API %%%%%%%%%%%%%%%

create(StructureId) -> spawn(fun() -> init(StructureId) end).


end_experiment(AGDS) -> AGDS ! end_experiment.



%% %%%%%%%%%%%%%%% Internal API %%%%%%%%%%%%%%%

notify_node_stimulated(AGDS, StimulatedNeighboursCount) -> 
    AGDS ! {node_stimulated, StimulatedNeighboursCount, self()},
    receive
        notification_processed -> ok
    end.

    

%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(StructureId) ->
    StructureIdAtom = list_to_atom(StructureId),

    erlang:register(StructureIdAtom, self()),
    pyrlang:send_client(StructureIdAtom, structure_created),

    Reporter = report:start(#{mode => pyrlang, structure_id => StructureIdAtom}),
    % Reporter = report:start(#{mode => silent, structure_id => StructureIdAtom}),
    GlobalCfg = #global_cfg{reporter=Reporter, dbg_counter=dbg_counter:create()},

    {ok, LogFile} = file:open("aas_ctrl.log", [write]),

    ONG = ong:create_ONG(self(), GlobalCfg),

    process_events(#state{
        structure_id = StructureIdAtom,
        node_groups = #node_groups{vngs = #{}, ong = ONG},
        transient_state = #transient_state{last_stimulation_id = none},
        configuration = #configuration{global_cfg = GlobalCfg},
        profiling_info = #profiling_info{},
        logging_info = #logging_info{log_file = LogFile}
    }).


%% %%%%%%%%%%%%%%% Main loop %%%%%%%%%%%%%%%

process_events(State) ->
    % io:format("AGDS waiting for message...~n"),
    receive
        Msg ->
            % io:format("Message received: ~p~n", [Msg]),
            handle_message(Msg, State)
    end.

handle_message(Msg, State) ->
    case Msg of
        subscription_init_ok ->
            process_events(State);

        {add_vng, Name, categorical} -> 
            NewState = add_vng_impl(Name, categorical, State),
            process_events(NewState);

        {add_vng, Name, numerical, Epsilon, MinValue, MaxValue} ->
            NewState = add_vng_impl(Name, numerical, Epsilon, MinValue, MaxValue, State),
            process_events(NewState);

        %% Values: #{VNGName := ObservedValue}
        {add_observation, ExperimentStep, Values} ->
            NewState = add_observation_impl(ExperimentStep, Values, State),
            process_events(NewState);

        %% VNValues: #{VNGName := Value}
        {get_on_for_exact_vn_values, VNValues} ->
            NewState = get_on_for_exact_vn_values(VNValues, State),
            process_events(NewState);

        %% ONIndex: integer, VNValues: #{VNGName := Value}
        {reconnect_on, ExperimentStep, ONIndex, ReconnectedVNValues, NewVNValues} ->
            NewState = reconnect_on_impl(ExperimentStep, ONIndex, ReconnectedVNValues, NewVNValues, State),
            process_events(NewState);

        % StimulationName: string, any name by which stimulaiton will be available in visualization
        % InitialStimuli: #{{vn, VNGName, Value} := Stimuli, {on, ONIndex} := Stimuli}
        % NodeGroupModes: #{VNGName => transitive | {responsive, excitation | value} | accumulative | passive}
        % MinPassedStimulus: float [0, 1]
        % VNToVNWeightMode, VNToONWeightMode: replacing | proportional
        {infere, ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, VNToVNWeightMode, VNToONWeightMode} ->
            {NewState, ElapsedTimeNative} = measure(fun() -> infere_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, VNToVNWeightMode, VNToONWeightMode, State) end),
            NewStateTimed = update_inference_time(NewState, ElapsedTimeNative),
            process_events(NewStateTimed);

        % InitianStimulation, NodeGroupModes, MinPassedStimulus: same as in infere
        {poison, ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinAccumulatedDose} ->
            NewState = poison_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinAccumulatedDose, State),
            process_events(NewState);

        {get_excitation, vng, VNGName} ->
            NewState = get_excitation_impl(vng, VNGName, State),
            process_events(NewState);

        {get_excitation, ong} ->
            NewState = get_excitation_impl(ong, State),
            process_events(NewState);

        {get_neighbours, vn, VNGName, Value} ->
            NewState = get_neighbours_impl(vn, VNGName, Value, State),
            process_events(NewState);

        {get_neighbours, on, ONIndex} ->
            NewState = get_neighbours_impl(on, ONIndex, State),
            process_events(NewState);

        {dbg_get_vns, VNGName, Caller} ->
            NewState = dbg_get_vns_impl(VNGName, Caller, State),
            process_events(NewState);

        {dbg_get_ons, Caller} ->
            NewState = dbg_get_ons_impl(Caller, State),
            process_events(NewState);

        get_structure_size ->
            NewState = get_structure_size_impl(State),
            process_events(NewState);

        get_inference_time_ms ->
            InferenceTimeMs = erlang:convert_time_unit(State#state.profiling_info#profiling_info.inference_time_native, native, millisecond),
            file:write(State#state.logging_info#logging_info.log_file, io_lib:format("Sending inference time: ~p~n", [{inference_time_ms, InferenceTimeMs}])),
            pyrlang:send_client(State#state.structure_id, {inference_time_ms, InferenceTimeMs}),
            process_events(State);

        reset_inference_time ->
            NewState = State#state{profiling_info = State#state.profiling_info#profiling_info{inference_time_native = 0}},
            pyrlang:send_client(State#state.structure_id, inference_time_zeroed),
            process_events(NewState);

        stop ->
            dbg_counter:print_report(State#state.configuration#configuration.global_cfg#global_cfg.dbg_counter),
            stop_impl(State)
    end.


measure(Fun) ->
    StartTime = erlang:monotonic_time(),
    NewState = Fun(),
    EndTime = erlang:monotonic_time(),
    ElapsedTimeNative = EndTime - StartTime,
    {NewState, ElapsedTimeNative}.


update_inference_time(#state{profiling_info = ProfilingInfo} = State, ElapsedTimeNative) ->
    CurrInferenceTimeNative = ProfilingInfo#profiling_info.inference_time_native,
    NewInferenceTimeNative = CurrInferenceTimeNative + ElapsedTimeNative,

    NewProfilingInfo = ProfilingInfo#profiling_info{inference_time_native = NewInferenceTimeNative},
    NewStateTimed = State#state{profiling_info = NewProfilingInfo},
    NewStateTimed.


add_vng_impl(Name, categorical, #state{node_groups = #node_groups{vngs = VNGs} = NodeGroups, configuration = #configuration{global_cfg = GlobalCfg}} = State) ->
    NewVNGs = VNGs#{Name => vng:create_categorical_VNG(Name, self(), GlobalCfg)},
    State#state{node_groups = NodeGroups#node_groups{vngs = NewVNGs}}.

add_vng_impl(Name, numerical, Epsilon, MinValue, MaxValue, #state{node_groups = #node_groups{vngs = VNGs} = NodeGroups, configuration = #configuration{global_cfg = GlobalCfg}} = State) ->
    NewVNGs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, MinValue, MaxValue, self(), GlobalCfg)},
    State#state{node_groups = NodeGroups#node_groups{vngs = NewVNGs}}.


add_observation_impl(ExperimentStep, Values, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}} = State) ->
    VNResults = ensure_vns_exist(Values, VNGs, ExperimentStep),
    AllExisting = check_if_all_vns_already_existed(VNResults),

    ONIndex = case AllExisting of
        true ->
            CommonONIndex = find_common_on_index(VNResults),
            case CommonONIndex of
                none -> 
                    NewONIndex = create_on_connected_to_vns(Values, VNGs, ExperimentStep, ONG),
                    NewONIndex;
                FoundONIndex ->
                    increment_on_occurances(FoundONIndex, ONG),
                    FoundONIndex
            end;
        false -> 
            NewONIndex = create_on_connected_to_vns(Values, VNGs, ExperimentStep, ONG),
            NewONIndex
    end,

    pyrlang:send_client(StructureId, {new_on_index, ONIndex}),
    State.


ensure_vns_exist(Values, VNGs, ExperimentStep) ->
    maps:foreach(fun(Name, Value) -> vng:ensure_vn(maps:get(Name, VNGs), Value, ExperimentStep) end, Values),
    maps:map(fun(Name, _Value) -> vng:wait_for_vn_ensured(maps:get(Name, VNGs)) end, Values).


check_if_all_vns_already_existed(VNResults) ->
    maps_util:all(fun(_Name, {_VN, IsNew}) -> IsNew =:= existing end, VNResults).


increment_on_occurances(ONIndex, ONG) ->
    ON = ong:get_ON(ONG, ONIndex),
    on:increment_occurances(ON).


create_on_connected_to_vns(VNValues, VNGs, ExperimentStep, ONG) ->
    {NewON, NewONIndex} = ong:new_ON(ExperimentStep, ONG),
    connect_all_vns_to_on(VNValues, VNGs, NewON, NewONIndex, ExperimentStep),
    NewONIndex.


find_common_on_index(VNResults) ->
    VNList = [VN || {VN, _IsNew} <- maps:values(VNResults)],
    case VNList of
        [] -> none;
        [FirstVN | RestVNs] ->
            FirstONIndices = sets:from_list(vn:get_neigh_on_indices(FirstVN)),
            CommonONIndices = lists:foldl(fun(VN, Acc) ->
                case sets:is_empty(Acc) of
                    true -> Acc;
                    false ->
                        ONIndices = sets:from_list(vn:get_neigh_on_indices(VN)),
                        sets:intersection(Acc, ONIndices)
                end
            end, FirstONIndices, RestVNs),

            case sets:size(CommonONIndices) of
                0 -> none;
                1 -> hd(sets:to_list(CommonONIndices)) 
            end
    end.


connect_all_vns_to_on(Values, VNGs, ON, ONIndex, ExperimentStep) ->
    maps:foreach(fun(Name, Value) -> vng:connect_vn_to_on(maps:get(Name, VNGs), Value, ON, ONIndex, ExperimentStep) end, Values),
    maps:foreach(fun(Name, _Value) -> vng:wait_for_vn_connected_to_on(maps:get(Name, VNGs)) end, Values).


reconnect_on_impl(ExperimentStep, ONIndex, _ReconnectedVNValues, NewVNValues, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}} = State) ->
    ON = ong:get_ON(ONG, ONIndex),

    % maps:foreach(fun(VNGName, Value) ->
    %     VNG = maps:get(VNGName, VNGs),
    %     vng:reconnect_vn_to_on(VNG, Value, ON, ONIndex, ExperimentStep)
    % end, ReconnectedVNValues),
    maps:foreach(fun(VNGName, Value) ->
        VNG = maps:get(VNGName, VNGs),
        vng:ensure_vn(VNG, Value, ExperimentStep)
    end, NewVNValues),
    NewVNs = maps:fold(fun(VNGName, _Value, Acc) -> 
        VNG = maps:get(VNGName, VNGs),
        {VN, _IsNew} = vng:wait_for_vn_ensured(VNG),
        Acc#{VNGName => VN}
    end, #{}, NewVNValues),

    connect_all_vns_to_on(NewVNValues, VNGs, ON, ONIndex, ExperimentStep),

    on:remove_outdated_connections(ON, NewVNs, ExperimentStep),

    pyrlang:send_client(StructureId, on_reconnected),
    State.


infere_impl(
    ExperimentStep, 
    StimulationName, 
    WriteToLog, 
    InitialStimuli, 
    NodeGroupModes, 
    MinPassedStimulus, 
    VNToVNWeightMode, 
    VNToONWeightMode, 
    #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}, configuration = #configuration{global_cfg = GlobalCfg}} = State
) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, GlobalCfg#global_cfg.reporter),

    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        stimulation_id=StimulationId, 
        experiment_step=ExperimentStep,
        stimulation_name=StimulationName,
        should_write_to_log=WriteToLog,
        stimulation_kind=inference, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        poisoning_params=#{}, 
        vn_to_vn_weight_mode=VNToVNWeightMode,
        vn_to_on_weight_mode=VNToONWeightMode
    },

    dbg_counter:add_inference(initial_stimulation_type(InitialStimuli), StimulationId, GlobalCfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),
    
    pyrlang:send_client(StructureId, inference_finished),
    State#state{transient_state = #transient_state{last_stimulation_id = StimulationId}}.
    

poison_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinimumAccumulatedDose, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}, configuration = #configuration{global_cfg = GlobalCfg}} = State) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, GlobalCfg#global_cfg.reporter),

    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        stimulation_id=StimulationId, 
        experiment_step=ExperimentStep,
        stimulation_name=StimulationName,
        should_write_to_log=WriteToLog,
        stimulation_kind=poisoning, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        poisoning_params=#{
            deadly_dose => DeadlyDose, 
            min_accumulated_dose => MinimumAccumulatedDose
        }
    },

    dbg_counter:add_inference(poison, StimulationId, GlobalCfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),

    pyrlang:send_client(StructureId, poisoning_finished),
    State#state{transient_state = #transient_state{last_stimulation_id = StimulationId}}.


get_excitation_impl(vng, VNGName, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs}, transient_state = #transient_state{last_stimulation_id = LastStimulationId}} = State) ->
    % other calls to maps:get(VNGName, VNGs) could be protected the same way
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> pyrlang:send_client(StructureId, {excitation_for_vng, non_existing_vng});
        VNG -> 
            VNsExcitation = vng:get_excitation(VNG, LastStimulationId),
            pyrlang:send_client(StructureId, {excitations, VNsExcitation})
    end,
    State.
    

get_excitation_impl(ong, #state{structure_id = StructureId, node_groups = #node_groups{ong = ONG}, transient_state = #transient_state{last_stimulation_id = LastStimulationId}} = State) ->
    ONsExcitation = ong:get_excitation(ONG, LastStimulationId),
    pyrlang:send_client(StructureId, {excitations, ONsExcitation}),
    State.
    

get_neighbours_impl(vn, VNGName, Value, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs}} = State) ->
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> pyrlang:send_client(StructureId, {neighbours, non_existing_vng});
        VNG -> 
            Neighbours = vng:get_neighbours(VNG, Value),
            pyrlang:send_client(StructureId, {neighbours, Neighbours})
    end,
    State.
    

get_neighbours_impl(on, ONIndex, #state{structure_id = StructureId, node_groups = #node_groups{ong = ONG}} = State) ->
    Neighs = ong:get_neighbours(ONG, ONIndex),
    pyrlang:send_client(StructureId, {neighbours, Neighs}),
    State.


dbg_get_vns_impl(VNGName, Caller, #state{node_groups = #node_groups{vngs = VNGs}} = State) ->
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> Caller ! {vns_for_vng, non_existing_vng};
        VNG -> 
            VNs = vng:get_all_vns(VNG),
            Caller ! {vns_for_vng, {VNGName, VNs}}
    end,
    State.


dbg_get_ons_impl(Caller, #state{node_groups = #node_groups{ong = ONG}} = State) ->
    ONIndices = ong:get_all_ON_indices(ONG),
    ONInfo = lists:map(fun(ONIndex) -> 
        ON = ong:get_ON(ONG, ONIndex),
        Neighs = on:get_neighbours(ON),
        {ONIndex, ON, Neighs}
    end, ONIndices),
    Caller ! {ons, ONInfo},
    State.


get_structure_size_impl(#state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}, logging_info = #logging_info{log_file = LogFile}} = State) ->
    VNGsSize = maps:fold(fun(_Name, VNG, Acc) -> Acc + vng:get_number_of_nodes(VNG) end, 0, VNGs),
    ONGSize = ong:get_number_of_nodes(ONG),
    file:write(LogFile, io_lib:format("Sending structure size: ~p~n", [VNGsSize + ONGSize])),
    pyrlang:send_client(StructureId, {structure_size, VNGsSize + ONGSize}),
    State.
    

get_on_for_exact_vn_values(VNValues, #state{structure_id = StructureId, node_groups = #node_groups{vngs = VNGs, ong = ONG}} = State) ->
    AllONIndices = ong:get_all_ON_indices(ONG),
    ONs = maps:fold(fun(VNGName, Value, Acc) -> 
        case sets:is_empty(Acc) of
            true -> Acc;
            false -> 
                VNG = maps:get(VNGName, VNGs),
                VN = vng:get_vn(VNG, Value),
                ConnectedONIndices = case VN of
                    none -> [];
                    _ -> vn:get_neigh_on_indices(VN)
                end,
                sets:intersection(Acc, sets:from_list(ConnectedONIndices))
        end
    end, sets:from_list(AllONIndices), VNValues),

    ONIndex = case sets:size(ONs) of
        0 -> none;
        1 -> hd(sets:to_list(ONs));
        Size -> 
            case maps:size(VNValues) < maps:size(VNGs) of
                true -> 
                    list_to_tuple(sets:to_list(ONs)); % returning tuple because returning list did not work with pyrlang
                false ->
                    ONsList = sets:to_list(ONs),
                    io:format("ERROR: Multiple ONs matched for exact VN values!~n"),
                    io:format("  VN Values: ~p~n", [VNValues]),
                    io:format("  Matched ONs count: ~p~n", [Size]),
                    io:format("  Matched ON indices: ~w~n", [ONsList]),
                    error({multiple_ons_for_exact_vn_values, VNValues, Size, ONsList})
            end
    end,

    pyrlang:send_client(StructureId, {on_for_exact_vn_values, ONIndex}),
    State.


stop_impl(#state{structure_id = StructureId} = State) ->
    pyrlang:send_client(StructureId, structure_stopped),
    delete_impl(State).



%% %%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%
 

stimulate(InitialStimuli, #stim_spec{node_group_modes=NodeGroupModes} = StimulationSpec, VNGs, ONG) ->
    StimuliByNodeGroup = maps:fold(fun(Target, Stimulus, Acc) -> 
        case Target of
            {vn, VNGName, Value} -> 
                case Acc of
                    #{{vng, VNGName} := AlreadyProcessedVNs} -> Acc#{{vng, VNGName} => AlreadyProcessedVNs#{Value => Stimulus}};
                    _ -> Acc#{{vng, VNGName} => #{Value => Stimulus}}
                end;
            {vng, VNGName} -> 
                Acc#{{vng, VNGName} => all_repr_value};
            {on, ONIndex} -> 
                case Acc of
                    #{ong := AlreadyProcessedONs} -> Acc#{ong => AlreadyProcessedONs#{ONIndex => Stimulus}};
                    _ -> Acc#{ong => #{ONIndex => Stimulus}}
                end
        end
    end, #{}, InitialStimuli),

    {ResponsiveNGStim, NonResponsiveNGStim} = utils:partition_map(fun(NodeGroup, _Stimuli) -> 
        case NodeGroup of
            {vng, VNGName} -> ng:is_responsive(VNGName, NodeGroupModes);
            ong -> ng:is_responsive("ong", NodeGroupModes)
        end
    end, StimuliByNodeGroup),
    
    stimulate_node_groups(ResponsiveNGStim, VNGs, ONG, StimulationSpec),
    stimulate_node_groups(NonResponsiveNGStim, VNGs, ONG, StimulationSpec).


delete_impl(#state{node_groups = #node_groups{vngs = VNGs, ong = ONG}, configuration = #configuration{global_cfg = #global_cfg{reporter = Reporter}}}) ->
    maps:foreach(fun(_Name, VNG) -> vng:delete(VNG) end, VNGs),
    ong:delete(ONG),
    report:stop(Reporter),
    ok.


wait_for_stimulation_to_finish(StimulatedNodeGroups, StimulationSpec, VNGs, ONG) ->
    case sets:is_empty(StimulatedNodeGroups) of
        true -> 
            ok;
        false ->
            receive
                {stimulation_finished, NodeGroup, 0, 1} -> wait_for_stimulation_to_finish(sets:del_element(NodeGroup, StimulatedNodeGroups), StimulationSpec, VNGs, ONG)
            end
    end.


initial_stimulation_type(InitialStimuli) -> 
    case lists:any(fun(StimulationSpec) ->
        case StimulationSpec of
            {vng, _VNGName} -> true;
            _ -> false
        end
    end, maps:keys(InitialStimuli)) 
    of
        true -> best_action_search;
        false -> action_value_search
    end.


stimulate_node_groups(StimuliByNodeGroup, VNGs, ONG, StimulationSpec) ->
    maps:foreach(fun(NodeGroup, Stimulations) -> 
        case NodeGroup of
            {vng, VNGName} -> vng:stimulate(maps:get(VNGName, VNGs), Stimulations, StimulationSpec);
            ong -> ong:stimulate(ONG, Stimulations, StimulationSpec)
        end
    end, StimuliByNodeGroup),

    StimulatedNodeGroups = maps:fold(fun(NodeGroup, _Stimuli, Acc) -> 
        case NodeGroup of
            {vng, VNGName} -> sets:add_element(maps:get(VNGName, VNGs), Acc);
            ong -> sets:add_element(ONG, Acc)
        end
    end, sets:new(), StimuliByNodeGroup),

    wait_for_stimulation_to_finish(StimulatedNodeGroups, StimulationSpec, VNGs, ONG).


% reset_after_deadlock(VNGs, ONG) ->
%     maps:foreach(fun(_Name, VNG) -> vng:reset_after_deadlock(VNG) end, VNGs),
%     ong:reset_after_deadlock(ONG).
