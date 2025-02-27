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
    receive
        subscription_init_ok ->
            process_events(State);

        {add_vng, Name, categorical} -> 
            NewState = add_vng_impl(Name, categorical, State),
            process_events(NewState);

        {add_vng, Name, numerical, Epsilon} ->
            NewState = add_vng_impl(Name, numerical, Epsilon, State),
            process_events(NewState);

        %% Values: #{VNGName := ObservedValue}
        {add_observation, ExperimentStep, Values} ->
            NewState = add_observation_impl(ExperimentStep, Values, State),
            process_events(NewState);

        % StimulationName: string, any name by which stimulaiton will be available in visualization
        % InitialStimuli: #{{vn, VNGName, Value} := Stimuli, {on, ONIndex} := Stimuli}
        % NodeGroupModes: #{VNGName => transitive | {responsive, excitation | value} | accumulative | passive}
        % MinPassedStimulus: float [0, 1]
        {infere, ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus} ->
            {NewState, ElapsedTimeNative} = measure(fun() -> infere_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, State) end),
            NewStateTimed = NewState#state{inference_time_native = State#state.inference_time_native + ElapsedTimeNative},
            file:write(State#state.log_file, io_lib:format("Total inference time: ~p~n", [erlang:convert_time_unit(NewStateTimed#state.inference_time_native, native, millisecond)])),
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

        get_structure_size ->
            NewState = get_structure_size_impl(State),
            process_events(NewState);

        get_inference_time_ms ->
            file:write(State#state.log_file, io_lib:format("Sending inference time: ~p~n", [{inference_time_ms, erlang:convert_time_unit(State#state.inference_time_native, native, millisecond)}])),
            pyrlang:send_client(State#state.structure_id, {inference_time_ms, erlang:convert_time_unit(State#state.inference_time_native, native, millisecond)}),
            process_events(State);

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

add_vng_impl(Name, numerical, Epsilon, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, self(), GlobalCfg)}}.


add_observation_impl(ExperimentStep, Values, #state{vngs = VNGs, ong = ONG, obs_count = ObsCount} = State) ->
    {NewON, NewONIndex} = ong:new_ON(ExperimentStep, ONG),
    maps:foreach(fun(Name, Value) -> vng:add_value(ExperimentStep, maps:get(Name, VNGs), Value, NewON, NewONIndex) end, Values),
    maps:foreach(fun(Name, _Value) -> vng:wait_for_value_added(maps:get(Name, VNGs)) end, Values),
    State#state{obs_count = ObsCount + 1}.


infere_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, State#state.global_cfg#global_cfg.reporter),

    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        id=StimulationId, 
        experiment_step=ExperimentStep,
        name=StimulationName,
        write_to_log=WriteToLog,
        kind=inference, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        params=#{}
    },

    dbg_counter:add_inference(initial_stimulation_type(InitialStimuli), StimulationId, State#state.global_cfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),
    
    pyrlang:send_client(StructureId, inference_finished),
    State#state{last_stimulation_id=StimulationId}.
    

poison_impl(ExperimentStep, StimulationName, WriteToLog, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinimumAccumulatedDose, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    report:node_group_modes(WriteToLog, NodeGroupModes, ExperimentStep, StimulationName, State#state.global_cfg#global_cfg.reporter),

    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        id=StimulationId, 
        experiment_step=ExperimentStep,
        name=StimulationName,
        write_to_log=WriteToLog,
        kind=poisoning, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        params=#{
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
    pyrlang:send_client(StructureId, {neighbours, ong:get_neighbours(ONG, ONIndex)}),
    State.


get_structure_size_impl(#state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    VNGsSize = maps:fold(fun(_Name, VNG, Acc) -> Acc + vng:get_number_of_nodes(VNG) end, 0, VNGs),
    ONGSize = ong:get_number_of_nodes(ONG),
    pyrlang:send_client(StructureId, {structure_size, VNGsSize + ONGSize}),
    State.
    

stop_impl(#state{structure_id = StructureId} = State) ->
    pyrlang:send_client(StructureId, structure_stopped),
    delete_impl(State).



%% %%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%
 

stimulate(InitialStimuli, StimulationSpec, VNGs, ONG) ->
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


% reset_after_deadlock(VNGs, ONG) ->
%     maps:foreach(fun(_Name, VNG) -> vng:reset_after_deadlock(VNG) end, VNGs),
%     ong:reset_after_deadlock(ONG).
