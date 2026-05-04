-module(agds).
-export([create/1, end_experiment/1]).
-export([notify_node_stimulated/2]).    % internal

-include("config.hrl").
-include("stimulation.hrl").

-record(state, {
    structure_id, 
    vngs = #{}, 
    ong, 
    global_cfg, 
    channel, 
    last_stimulation_id=none, 
    obs_count=0, 
    is_profiling=false,
    inference_time_native=0,
    log_file
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

    process_events(#state{structure_id = StructureIdAtom, ong = ong:create_ONG(self(), GlobalCfg), global_cfg = GlobalCfg, log_file=LogFile}).


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
        {infere, ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus} ->
            {NewState, ElapsedTimeNative} = measure(fun() -> infere_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, State) end),
            NewStateTimed = NewState#state{inference_time_native = State#state.inference_time_native + ElapsedTimeNative},
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
            file:write(State#state.log_file, io_lib:format("Sending inference time: ~p~n", [{inference_time_ms, erlang:convert_time_unit(State#state.inference_time_native, native, millisecond)}])),
            pyrlang:send_client(State#state.structure_id, {inference_time_ms, erlang:convert_time_unit(State#state.inference_time_native, native, millisecond)}),
            process_events(State);

        reset_inference_time ->
            NewState = State#state{inference_time_native=0},
            pyrlang:send_client(State#state.structure_id, inference_time_zeroed),
            process_events(NewState);

        stop ->
            dbg_counter:print_report(State#state.global_cfg#global_cfg.dbg_counter),
            stop_impl(State)
    end.


measure(Fun) ->
    StartTime = erlang:monotonic_time(),
    NewState = Fun(),
    EndTime = erlang:monotonic_time(),
    ElapsedTimeNative = EndTime - StartTime,
    {NewState, ElapsedTimeNative}.


add_vng_impl(Name, categorical, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_categorical_VNG(Name, self(), GlobalCfg)}}.

add_vng_impl(Name, numerical, Epsilon, MinValue, MaxValue, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, MinValue, MaxValue, self(), GlobalCfg)}}.


add_observation_impl(ExperimentStep, Values, #state{vngs = VNGs, ong = ONG, obs_count = ObsCount} = State) ->
    {NewON, NewONIndex} = ong:new_ON(ExperimentStep, ONG),
    maps:foreach(fun(Name, Value) -> vng:add_value(ExperimentStep, maps:get(Name, VNGs), Value, NewON, NewONIndex) end, Values),
    maps:foreach(fun(Name, _Value) -> vng:wait_for_value_added(maps:get(Name, VNGs)) end, Values),

    pyrlang:send_client(State#state.structure_id, {new_on_index, NewONIndex}),
    State#state{obs_count = ObsCount + 1}.


reconnect_on_impl(ExperimentStep, ONIndex, _ReconnectedVNValues, NewVNValues, #state{vngs = VNGs, ong = ONG} = State) ->
    ON = ong:get_ON(ONG, ONIndex),

    % maps:foreach(fun(VNGName, Value) ->
    %     VNG = maps:get(VNGName, VNGs),
    %     vng:reconnect_vn_to_on(VNG, Value, ON, ONIndex, ExperimentStep)
    % end, ReconnectedVNValues),
    maps:foreach(fun(VNGName, Value) ->
        VNG = maps:get(VNGName, VNGs),
        vng:add_value(ExperimentStep, VNG, Value, ON, ONIndex)
    end, NewVNValues),
    NewVNs = maps:fold(fun(VNGName, _Value, Acc) -> 
        VNG = maps:get(VNGName, VNGs),
        {ok, VN} = vng:wait_for_value_added(VNG),
        Acc#{VNGName => VN}
    end, #{}, NewVNValues),

    on:remove_outdated_connections(ON, NewVNs, ExperimentStep),

    pyrlang:send_client(State#state.structure_id, on_reconnected),
    State.


infere_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, State#state.global_cfg#global_cfg.reporter),

    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        stimulation_id=StimulationId, 
        experiment_step=ExperimentStep,
        stimulation_name=StimulationName,
        should_write_to_log=WriteToLog,
        stimulation_kind=inference, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        poisoning_params=#{}
    },

    dbg_counter:add_inference(initial_stimulation_type(InitialStimuli), StimulationId, State#state.global_cfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),
    
    pyrlang:send_client(StructureId, inference_finished),
    State#state{last_stimulation_id=StimulationId}.
    

poison_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinimumAccumulatedDose, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, State#state.global_cfg#global_cfg.reporter),

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

    dbg_counter:add_inference(poison, StimulationId, State#state.global_cfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),

    pyrlang:send_client(StructureId, poisoning_finished),
    State#state{last_stimulation_id=StimulationId}.


get_excitation_impl(vng, VNGName, #state{structure_id = StructureId, vngs = VNGs, last_stimulation_id=LastStimulationId} = State) ->
    % other calls to maps:get(VNGName, VNGs) could be protected the same way
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> pyrlang:send_client(StructureId, {excitation_for_vng, non_existing_vng});
        VNG -> 
            VNsExcitation = vng:get_excitation(VNG, LastStimulationId),
            pyrlang:send_client(StructureId, {excitations, VNsExcitation})
    end,
    State.
    

get_excitation_impl(ong, #state{structure_id = StructureId, ong = ONG, last_stimulation_id=LastStimulationId} = State) ->
    ONsExcitation = ong:get_excitation(ONG, LastStimulationId),
    pyrlang:send_client(StructureId, {excitations, ONsExcitation}),
    State.
    

get_neighbours_impl(vn, VNGName, Value, #state{structure_id = StructureId, vngs = VNGs} = State) ->
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> pyrlang:send_client(StructureId, {neighbours, non_existing_vng});
        VNG -> 
            Neighbours = vng:get_neighbours(VNG, Value),
            pyrlang:send_client(StructureId, {neighbours, Neighbours})
    end,
    State.
    

get_neighbours_impl(on, ONIndex, #state{structure_id = StructureId, ong = ONG} = State) ->
    Neighs = ong:get_neighbours(ONG, ONIndex),
    pyrlang:send_client(StructureId, {neighbours, Neighs}),
    State.


dbg_get_vns_impl(VNGName, Caller, #state{vngs = VNGs} = State) ->
    case maps:get(VNGName, VNGs, non_existing_vng) of
        non_existing_vng -> Caller ! {vns_for_vng, non_existing_vng};
        VNG -> 
            VNs = vng:get_all_vns(VNG),
            Caller ! {vns_for_vng, {VNGName, VNs}}
    end,
    State.


dbg_get_ons_impl(Caller, #state{ong = ONG} = State) ->
    ONIndices = ong:get_all_ON_indices(ONG),
    ONInfo = lists:map(fun(ONIndex) -> 
        ON = ong:get_ON(ONG, ONIndex),
        Neighs = on:get_neighbours(ON),
        {ONIndex, ON, Neighs}
    end, ONIndices),
    Caller ! {ons, ONInfo},
    State.


get_structure_size_impl(#state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    VNGsSize = maps:fold(fun(_Name, VNG, Acc) -> Acc + vng:get_number_of_nodes(VNG) end, 0, VNGs),
    ONGSize = ong:get_number_of_nodes(ONG),
    file:write(State#state.log_file, io_lib:format("Sending structure size: ~p~n", [VNGsSize + ONGSize])),
    pyrlang:send_client(StructureId, {structure_size, VNGsSize + ONGSize}),
    State.
    

get_on_for_exact_vn_values(VNValues, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
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


delete_impl(#state{vngs = VNGs, ong = ONG, global_cfg = #global_cfg{reporter = Reporter}}) ->
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
