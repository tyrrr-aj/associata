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
    is_profiling=false
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
    io:format("AGDS started~n", []),

    StructureIdAtom = list_to_atom(StructureId),

    erlang:register(StructureIdAtom, self()),
    pyrlang:send_client(StructureIdAtom, structure_created),

    Reporter = report:start(#{mode => pyrlang, structure_id => StructureIdAtom}),
    GlobalCfg = #global_cfg{reporter=Reporter, dbg_counter=dbg_counter:create()},

    process_events(#state{structure_id = StructureIdAtom, ong = ong:create_ONG(self(), GlobalCfg), global_cfg = GlobalCfg}).


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
        {add_observation, Values} ->
            NewState = add_observation_impl(Values, State),
            process_events(NewState);

        % InitialStimuli: #{{vn, VNGName, Value} := Stimuli, {on, ONIndex} := Stimuli}
        % NodeGroupModes: #{VNGName => transitive | {responsive, excitation | value} | accumulative | passive}
        % MinPassedStimulus: float [0, 1]
        {infere, InitialStimuli, NodeGroupModes, MinPassedStimulus} ->
            % io:format("~s    AGDS: infere~n", [utils:get_timestamp_str()]),
            NewState = infere_impl(InitialStimuli, NodeGroupModes, MinPassedStimulus, State),
            process_events(NewState);

        % InitianStimulation, NodeGroupModes, MinPassedStimulus: same as in infere
        {poison, InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinAccumulatedDose} ->
            % io:format("~s    AGDS: poison~n", [utils:get_timestamp_str()]),
            NewState = poison_impl(InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinAccumulatedDose, State),
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

        stop ->
            dbg_counter:print_report(State#state.global_cfg#global_cfg.dbg_counter),
            stop_impl(State)
    end.



add_vng_impl(Name, categorical, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_categorical_VNG(Name, self(), GlobalCfg)}}.

add_vng_impl(Name, numerical, Epsilon, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, self(), GlobalCfg)}}.


add_observation_impl(Values, #state{vngs = VNGs, ong = ONG, obs_count = ObsCount} = State) ->
    {NewON, NewONIndex} = ong:new_ON(ONG),
    maps:foreach(fun(Name, Value) -> vng:add_value(maps:get(Name, VNGs), Value, NewON, NewONIndex) end, Values),
    State#state{obs_count = ObsCount + 1}.


infere_impl(InitialStimuli, NodeGroupModes, MinPassedStimulus, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        id=StimulationId, 
        kind=inference, 
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        params=#{}
    },

    dbg_counter:add_inference(initial_stimulation_type(InitialStimuli), StimulationId, State#state.global_cfg#global_cfg.dbg_counter),
    stimulate(InitialStimuli, StimulationSpec, VNGs, ONG),
    
    pyrlang:send_client(StructureId, inference_finished),
    State#state{last_stimulation_id=StimulationId}.
    

poison_impl(InitialStimuli, NodeGroupModes, MinPassedStimulus, DeadlyDose, MinimumAccumulatedDose, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    StimulationId = erlang:unique_integer(),
    StimulationSpec = #stim_spec{
        id=StimulationId, 
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
    

stop_impl(#state{structure_id = StructureId, global_cfg = GlobalCfg} = State) ->
    save_experiment(GlobalCfg),
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

    wait_for_stimulation_to_finish(StimulatedNodeGroups).


delete_impl(#state{vngs = VNGs, ong = ONG, global_cfg = #global_cfg{reporter = Reporter}}) ->
    maps:foreach(fun(_Name, VNG) -> vng:delete(VNG) end, VNGs),
    ong:delete(ONG),
    report:stop(Reporter),
    ok.


save_experiment(#global_cfg{reporter = Reporter}) -> report:experiment_end(Reporter).


wait_for_stimulation_to_finish(StimulatedNodeGroups) ->
    % io:format("~s    AGDS: waiting for stimulation to finish, StimulatedNodeGroups: ~p~n", [utils:get_timestamp_str(), sets:to_list(StimulatedNodeGroups)]),
    case sets:is_empty(StimulatedNodeGroups) of
        true -> 
            % io:format("~s    AGDS: Stimulation finished~n", [utils:get_timestamp_str()]),
            ok;
        false ->
            receive
                {stimulation_finished, NodeGroup, 0, 1} -> wait_for_stimulation_to_finish(sets:del_element(NodeGroup, StimulatedNodeGroups))
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
