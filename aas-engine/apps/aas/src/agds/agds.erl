-module(agds).
-export([create/1, add_VNG/4, add_observation/2, infere/3, reset_excitation/1, end_experiment/1, delete/1]).
-export([poison/4]).
-export([get_excitation_for_vng/2]).
-export([notify_node_stimulated/2]).

-include("config.hrl").

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


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create(StructureId) -> 
    spawn(fun() -> init(StructureId) end).


%% VNGType: categorical | numerical
add_VNG(AGDS, Name, VNGType, IsAction) -> AGDS ! {add_VNG, Name, VNGType, IsAction}.

%% Values: #{VNGName := ObservedValue}
add_observation(AGDS, Values) -> AGDS ! {add_observation, Values}.

%% InitialStimulation: #{{vn, VNGName, Value} := Stimuli, {on, ONIndex} := Stimuli}
infere(AGDS, InitialStimulation, MaxInferenceDepth) -> AGDS ! {infere, InitialStimulation, MaxInferenceDepth}.


%% kill ONs that are closely associated with given input
poison(AGDS, InitialStimulation, MaxDepth, DeadlyDose) ->
    AGDS ! {poison, InitialStimulation, MaxDepth, DeadlyDose}.


get_excitation_for_vng(AGDS, VNGName) ->
    AGDS ! {get_excitation_for_vng, VNGName},
    receive
        {excitation_for_vng, Excitation} -> Excitation
    end.


reset_excitation(AGDS) -> AGDS ! reset_excitation.


end_experiment(AGDS) -> AGDS ! end_experiment.


delete(AGDS) -> AGDS ! delete.


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
    TimestepMs = 5,
    GlobalCfg = #global_cfg{reporter=Reporter, timestep_ms=TimestepMs, dbg_counter=dbg_counter:create()},

    % eprof:start(),
    % eprof:start_profiling([self()]),

    process_events(#state{structure_id = StructureIdAtom, ong = ong:create_ONG(self(), GlobalCfg), global_cfg = GlobalCfg}).


%% %%%%%%%%%%%%%%% Main loop %%%%%%%%%%%%%%%

% process_events(#state{obs_count = 1500, is_profiling=false} = State) ->
%     % io:format("AGDS: 90 observations reached, starting profiling...~n", []),
%     eflame:apply(fun process_events/1, [State#state{is_profiling=true}]),
%     % io:format("AGDS: profiling finished~n", []);
%     timer:sleep(3000);


process_events(State) ->
    % case State#state.obs_count rem 10 of
    %     0 -> io:format("AGDS: ~p observations processed~n", [State#state.obs_count]);
    %     _ -> ok
    % end,

    receive
        subscription_init_ok ->
            process_events(State);

        {add_vng, Name, categorical, IsAction} -> 
            NewState = add_vng_impl(Name, categorical, IsAction, State),
            process_events(NewState);

        {add_vng, Name, numerical, Epsilon, IsAction} ->
            NewState = add_vng_impl(Name, numerical, Epsilon, IsAction, State),
            process_events(NewState);

        {add_observation, Values} ->
            NewState = add_observation_impl(Values, State),
            process_events(NewState);

        {infere, InitialStimulation, MaxInferenceDepth} ->
            NewState = infere_impl(InitialStimulation, MaxInferenceDepth, State),
            process_events(NewState);

        {poison, InitialStimulation, MaxDepth, DeadlyDose, MinimumAccumulatedDose} ->
            NewState = poison_impl(InitialStimulation, MaxDepth, DeadlyDose, MinimumAccumulatedDose, State),
            process_events(NewState);

        {get_excitation, vng, VNGName} ->
            NewState = get_excitation_impl(vng, VNGName, State),
            process_events(NewState);

        {get_excitation, ong} ->
            NewState = get_excitation_impl(ong, State),
            process_events(NewState);

        reset_excitation ->
            NewState = reset_excitation_impl(State),
            process_events(NewState);

        {get_neighbours, vn, VNGName, Value} ->
            NewState = get_neighbours_impl(vn, VNGName, Value, State),
            process_events(NewState);

        {get_neighbours, on, ONIndex} ->
            NewState = get_neighbours_impl(on, ONIndex, State),
            process_events(NewState);

        stop ->
            % eprof:stop_profiling(),
            % eprof:log("eprof.txt"),
            % eprof:analyze(total),
            % eprof:stop()
             
            % lists:foreach(fun(NodeGroup) ->
            %     NodeGroup ! {report_stimulations_count, self()},
            %     receive
            %         {stimulations_count_reported, NodeGroup} -> ok
            %     end
            % end, [State#state.ong | maps:values(State#state.vngs)]),

            dbg_counter:print_report(State#state.global_cfg#global_cfg.dbg_counter),

            stop_impl(State)
    end.



add_vng_impl(Name, categorical, IsAction, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_categorical_VNG(Name, IsAction, self(), GlobalCfg)}}.

add_vng_impl(Name, numerical, Epsilon, IsAction, #state{vngs = VNGs, global_cfg = GlobalCfg} = State) ->
    State#state{vngs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, IsAction, self(), GlobalCfg)}}.


add_observation_impl(Values, #state{vngs = VNGs, ong = ONG, obs_count = ObsCount} = State) ->
    NewON = ong:new_ON(ONG),
    maps:foreach(fun(Name, Value) -> vng:add_value(maps:get(Name, VNGs), Value, NewON) end, Values),
    State#state{obs_count = ObsCount + 1}.


infere_impl(InitialStimulation, MaxInferenceDepth, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    StimulationId = erlang:unique_integer(),
    StimulationKind = {infere, StimulationId},

    dbg_counter:add_inference(initial_stimulation_type(InitialStimulation), StimulationId, State#state.global_cfg#global_cfg.dbg_counter),

    stimulate(InitialStimulation, MaxInferenceDepth, StimulationKind, VNGs, ONG),

    % InitStimulatedNodesCount = maps:fold(fun(Target, Stimuli, Acc) -> Acc + stimulate(Target, Stimuli, MaxInferenceDepth, VNGs, ONG, StimulationKind) end, 0, InitialStimulation),
    % timer:sleep((TimestepMs + 5) * MaxInferenceDepth),
    % wait_for_inference_to_finish(InitStimulatedNodesCount),
    
    pyrlang:send_client(StructureId, inference_finished),
    State#state{last_stimulation_id=StimulationId}.
    

poison_impl(InitialStimulation, MaxDepth, DeadlyDose, MinimumAccumulatedDose, #state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    StimulationId = erlang:unique_integer(),
    StimulationKind = {poison, DeadlyDose, MinimumAccumulatedDose, StimulationId},

    dbg_counter:add_inference(poison, StimulationId, State#state.global_cfg#global_cfg.dbg_counter),

    stimulate(InitialStimulation, MaxDepth, StimulationKind, VNGs, ONG),

    % InitStimulatedNodesCount = maps:fold(fun(Target, Stimuli, Acc) -> Acc + stimulate(Target, Stimuli, MaxDepth, VNGs, ONG, StimulationKind) end, 0, InitialStimulation),
    % timer:sleep((TimestepMs + 5) * MaxDepth),
    % wait_for_inference_to_finish(InitStimulatedNodesCount),
    
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
    

reset_excitation_impl(#state{structure_id = StructureId, vngs = VNGs, ong = ONG} = State) ->
    maps:foreach(fun(_Name, VNG) -> vng:reset_excitation(VNG) end, VNGs),
    ong:reset_excitation(ONG),

    maps:foreach(fun(_Name, VNG) -> vng:wait_for_reset_excitation(VNG) end, VNGs),
    ong:wait_for_reset_excitation(ONG),

    pyrlang:send_client(StructureId, excitation_reset_finished),
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
 

% stimulate({vn, VNGName, Value}, Stimuli, MaxInferenceDepth, VNGs, _ONG, StimulationKind) -> vng:stimulate(maps:get(VNGName, VNGs), Value, Stimuli, MaxInferenceDepth, StimulationKind);

% stimulate({on, ONIndex}, Stimuli, MaxInferenceDepth, _VNGs, ONG, StimulationKind) -> ong:stimulate(ONG, ONIndex, Stimuli, MaxInferenceDepth, StimulationKind);

% stimulate({vng, VNGName}, repr_value, MaxInferenceDepth, VNGs, _ONG, StimulationKind) -> vng:stimulate(maps:get(VNGName, VNGs), all, repr_value, MaxInferenceDepth, StimulationKind).


stimulate(InitialStimulation, MaxInferenceDepth, StimulationKind, VNGs, ONG) ->
%     io:format("Stimulating (~p) with InitialStimulation: ~p~n", [StimulationKind, InitialStimulation]),

    StimulationsByNodeGroup = maps:fold(fun(Target, Stimuli, Acc) -> 
        case Target of
            {vn, VNGName, Value} -> 
                case Acc of
                    #{{vng, VNGName} := AlreadyProcessedVNs} -> Acc#{{vng, VNGName} => AlreadyProcessedVNs#{Value => Stimuli}};
                    _ -> Acc#{{vng, VNGName} => #{Value => Stimuli}}
                end;
            {vng, VNGName} -> 
                Acc#{{vng, VNGName} => all_repr_value};
            {on, ONIndex} -> 
                case Acc of
                    #{ong := AlreadyProcessedONs} -> Acc#{ong => AlreadyProcessedONs#{ONIndex => Stimuli}};
                    _ -> Acc#{ong => #{ONIndex => Stimuli}}
                end
        end
    end, #{}, InitialStimulation),

    
    maps:foreach(fun(NodeGroup, Stimulations) -> 
        case NodeGroup of
            {vng, VNGName} -> vng:stimulate(maps:get(VNGName, VNGs), Stimulations, MaxInferenceDepth, StimulationKind);
            ong -> ong:stimulate(ONG, Stimulations, MaxInferenceDepth, StimulationKind)
        end
    end, StimulationsByNodeGroup),

    StimulatedNodeGroups = maps:fold(fun(NodeGroup, _Stimulations, Acc) -> 
        case NodeGroup of
            {vng, VNGName} -> sets:add_element(maps:get(VNGName, VNGs), Acc);
            ong -> sets:add_element(ONG, Acc)
        end
    end, sets:new(), StimulationsByNodeGroup),

    wait_for_inference_to_finish(StimulatedNodeGroups).


delete_impl(#state{vngs = VNGs, ong = ONG, global_cfg = #global_cfg{reporter = Reporter}}) ->
    maps:foreach(fun(_Name, VNG) -> vng:delete(VNG) end, VNGs),
    ong:delete(ONG),
    report:stop(Reporter),
    ok.


save_experiment(#global_cfg{reporter = Reporter}) -> report:experiment_end(Reporter).


% wait_for_inference_to_finish(0) -> 
%     ok;

% wait_for_inference_to_finish(ExpectedStimulationsCount) ->
%     receive
%         {node_stimulated, StimulatedNeighboursCount, Notifier} ->
%             % io:format("Received node_stimulated message with StimulatedNeighboursCount (ExpectedStimulationsCount: ~p): ~p~n", [ExpectedStimulationsCount, StimulatedNeighboursCount]),
%             NewExpectedStimulationsCount = ExpectedStimulationsCount + StimulatedNeighboursCount - 1,
%             Notifier ! notification_processed,
%             wait_for_inference_to_finish(NewExpectedStimulationsCount)
%     end.

% NEW VERSION: recursive processing of node_stimulated messages


wait_for_inference_to_finish(StimulatedNodeGroups) ->
    % io:format("Waiting for inference to finish, StimulatedNodeGroups: ~p~n", [sets:to_list(StimulatedNodeGroups)]),
    case sets:is_empty(StimulatedNodeGroups) of
        true -> ok;
        false ->
            receive
                {stimulation_finished, NodeGroup} -> wait_for_inference_to_finish(sets:del_element(NodeGroup, StimulatedNodeGroups))
            end
    end.


%% POTENTIAL OPTIMIZATION (to old, non-recursive version) - async processing of node_stimulated messages
% wait_for_inference_to_finish(0) -> 
%     ok;

% wait_for_inference_to_finish(InitExpectedStimulationsCount) ->
%     wait_for_inference_to_finish_loop(#{0 => InitExpectedStimulationsCount}).


% wait_for_inference_to_finish_loop(#{}) -> ok;

% wait_for_inference_to_finish_loop(ExpectedStimulationsCounts) ->
%     receive
%         {node_stimulated, StimulatedNeighboursCount, CurrInferenceDepth} ->
%             StimulationsCountForCurrDepth = maps:get(CurrInferenceDepth, ExpectedStimulationsCounts, 0),
%             NewStimulationsCountForCurrDepth = StimulationsCountForCurrDepth - 1,
%             ExpectedStimulationsCountsUpdatedForCurrDepth = if 
%                 NewStimulationsCountForCurrDepth == 0 -> maps:remove(CurrInferenceDepth, ExpectedStimulationsCounts);
%                 true -> ExpectedStimulationsCounts#{CurrInferenceDepth => NewStimulationsCountForCurrDepth}
%             end,

%             NextInferenceDepth = CurrInferenceDepth + 1,            
%             StimulationsCountForNextDepth = maps:get(NextInferenceDepth, ExpectedStimulationsCounts, 0),
%             NewStimulationsCountForNextDepth = StimulationsCountForNextDepth + StimulatedNeighboursCount,
%             ExpectedStimulationsCountsUpdatedForNextDepth = if
%                 NewStimulationsCountForNextDepth == 0 -> maps:remove(NextInferenceDepth, ExpectedStimulationsCountsUpdatedForCurrDepth);
%                 true -> ExpectedStimulationsCountsUpdatedForCurrDepth#{NextInferenceDepth => NewStimulationsCountForNextDepth}
%             end,

%             wait_for_inference_to_finish_loop(ExpectedStimulationsCountsUpdatedForNextDepth)
%     end.


initial_stimulation_type(InitialStimulation) -> 
    case lists:any(fun(StimSpec) ->
        case StimSpec of
            {vng, _VNGName} -> true;
            _ -> false
        end
    end, maps:keys(InitialStimulation)) 
    of
        true -> best_action_search;
        false -> action_value_search
    end.
