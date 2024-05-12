-module(agds).
-export([create/2, add_VNG/4, add_observation/2, infere/3, reset_excitation/1, end_experiment/1, delete/1]).
-export([poison/4]).
-export([get_excitation_for_vng/2]).
-export([notify_node_stimulated/2]).

-include("config.hrl").

-record(state, {vngs = #{}, ong, global_cfg, channel, total_infere_time=0.0, total_poison_time=0.0}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create(StructureId, Connection) -> spawn(fun() -> init(StructureId, Connection) end).


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

init(StructureId, Connection) ->
    io:format("AGDS started~n", []),

    Channel = rabbitmq:setup_channel(Connection, StructureId),
    rabbitmq:respond("structure_created", Channel),

    Reporter = report:start(#{mode => rabbitmq, channel => Channel}),
    TimestepMs = 5,
    GlobalCfg = #global_cfg{reporter=Reporter, timestep_ms=TimestepMs},

    process_events(#state{ong = ong:create_ONG(self(), GlobalCfg), global_cfg = GlobalCfg, channel = Channel}).


process_events(#state{vngs = VNGs, ong = ONG, global_cfg = #global_cfg{timestep_ms = TimestepMs} = GlobalCfg, channel = Channel, total_infere_time=TotalInfereTime, total_poison_time=TotalPoisonTime} = State) ->
    receive
        Cmd -> 
            Message = rabbitmq:decode_and_ack_message(Cmd, Channel),

            case Message of
                subscription_init_ok ->
                    process_events(State);

                {add_vng, Name, categorical, IsAction} -> 
                    process_events(#state{vngs = VNGs#{Name => vng:create_categorical_VNG(Name, IsAction, self(), GlobalCfg)}, ong = ONG, global_cfg = GlobalCfg, channel = Channel});

                {add_vng, Name, numerical, Epsilon, IsAction} ->
                    process_events(#state{vngs = VNGs#{Name => vng:create_numerical_VNG(Name, Epsilon, IsAction, self(), GlobalCfg)}, ong = ONG, global_cfg = GlobalCfg, channel = Channel});

                {add_observation, Values} ->
                    ST = print_start(add_observation),
                    NewON = ong:new_ON(ONG),
                    maps:foreach(fun(Name, Value) -> vng:add_value(maps:get(Name, VNGs), Value, NewON) end, Values),
                    print_end(ST),
                    process_events(State);

                {infere, InitialStimulation, MaxInferenceDepth} ->
                    CallStartTime = erlang:monotonic_time(millisecond),
                    ST = print_start(infere),
                    StimulationKind = infere,
                    InitStimulatedNodesCount = maps:fold(fun(Target, Stimuli, Acc) -> Acc + stimulate(Target, Stimuli, MaxInferenceDepth, VNGs, ONG, StimulationKind) end, 0, InitialStimulation),
                    % timer:sleep((TimestepMs + 5) * MaxInferenceDepth),
                    wait_for_inference_to_finish(InitStimulatedNodesCount),
                    rabbitmq:respond("inference_finished", Channel),
                    print_end(ST),
                    CallElapsedTime = erlang:monotonic_time(millisecond) - CallStartTime,
                    process_events(State#state{total_infere_time = TotalInfereTime + CallElapsedTime});

                {poison, InitialStimulation, MaxDepth, DeadlyDose, MinimumAccumulatedDose} ->
                    CallStartTime = erlang:monotonic_time(millisecond),
                    ST = print_start(poison),
                    StimulationKind = {poison, DeadlyDose, MinimumAccumulatedDose, erlang:unique_integer([positive])},
                    InitStimulatedNodesCount = maps:fold(fun(Target, Stimuli, Acc) -> Acc + stimulate(Target, Stimuli, MaxDepth, VNGs, ONG, StimulationKind) end, 0, InitialStimulation),
                    % timer:sleep((TimestepMs + 5) * MaxDepth),
                    wait_for_inference_to_finish(InitStimulatedNodesCount),
                    rabbitmq:respond("poisoning_finished", Channel),
                    print_end(ST),
                    CallElapsedTime = erlang:monotonic_time(millisecond) - CallStartTime,
                    process_events(State#state{total_poison_time = TotalPoisonTime + CallElapsedTime});

                {get_excitation, vng, VNGName} ->
                    ST = print_start(get_excitation_vng),
                    % other calls to maps:get(VNGName, VNGs) could be protected the same way
                    case maps:get(VNGName, VNGs, non_existing_vng) of
                        non_existing_vng -> rabbitmq:respond({excitation_for_vng, non_existing_vng}, Channel);
                        VNG -> 
                            VNsExcitation = vng:get_excitation(VNG),
                            rabbitmq:respond({excitations, VNsExcitation}, Channel)
                    end,
                    print_end(ST),
                    process_events(State);

                {get_excitation, ong} ->
                    ST = print_start(get_excitation_ong),
                    ONsExcitation = ong:get_excitation(ONG),
                    rabbitmq:respond({excitations, ONsExcitation}, Channel),
                    print_end(ST),
                    process_events(State);

                reset_excitation ->
                    ST = print_start(reset_excitation),
                    maps:foreach(fun(_Name, VNG) -> vng:reset_excitation(VNG) end, VNGs),
                    ong:reset_excitation(ONG),
                    rabbitmq:respond("excitation_reset_finished", Channel),
                    print_end(ST),
                    process_events(State);

                {get_neighbours, vn, VNGName, Value} ->
                    ST = print_start(get_neighbours_vn),
                    case maps:get(VNGName, VNGs, non_existing_vng) of
                        non_existing_vng -> rabbitmq:respond({neighbours, non_existing_vng}, Channel);
                        VNG -> 
                            Neighbours = vng:get_neighbours(VNG, Value),
                            rabbitmq:respond({neighbours, Neighbours}, Channel)
                    end,
                    print_end(ST),
                    process_events(State);

                {get_neighbours, on, ONIndex} ->
                    ST = print_start(get_neighbours_on),
                    rabbitmq:respond({neighbours, ong:get_neighbours(ONG, ONIndex)}, Channel),
                    print_end(ST),
                    process_events(State);

                stop ->
                    file:write_file(
                        "C:\\Users\\adams\\Doktorat\\aasociata\\pyassoc\\backend_profile.txt", 
                        io_lib:fwrite("Total infere time: ~f~nTotal poison time: ~f~n", [TotalInfereTime / 1000, TotalPoisonTime / 1000])
                    ),
                    save_experiment(GlobalCfg),
                    rabbitmq:respond("structure_stopped", Channel),
                    delete_impl(State)
            end
    end.
 

stimulate({vn, VNGName, Value}, Stimuli, MaxInferenceDepth, VNGs, _ONG, StimulationKind) -> vng:stimulate(maps:get(VNGName, VNGs), Value, Stimuli, MaxInferenceDepth, StimulationKind);

stimulate({on, ONIndex}, Stimuli, MaxInferenceDepth, _VNGs, ONG, StimulationKind) -> ong:stimulate(ONG, ONIndex, Stimuli, MaxInferenceDepth, StimulationKind);

stimulate({vng, VNGName}, repr_value, MaxInferenceDepth, VNGs, _ONG, StimulationKind) -> vng:stimulate(maps:get(VNGName, VNGs), all, repr_value, MaxInferenceDepth, StimulationKind).


delete_impl(#state{vngs = VNGs, ong = ONG, global_cfg = #global_cfg{reporter = Reporter}, channel = Channel}) ->
    maps:foreach(fun(_Name, VNG) -> vng:delete(VNG) end, VNGs),
    ong:delete(ONG),
    report:stop(Reporter),
    rabbitmq:teardown_channel(Channel).


save_experiment(#global_cfg{reporter = Reporter}) -> report:experiment_end(Reporter).


wait_for_inference_to_finish(0) -> 
    ok;

wait_for_inference_to_finish(ExpectedStimulationsCount) ->
    receive
        {node_stimulated, StimulatedNeighboursCount, Notifier} ->
            % io:format("Received node_stimulated message with StimulatedNeighboursCount (ExpectedStimulationsCount: ~p): ~p~n", [ExpectedStimulationsCount, StimulatedNeighboursCount]),
            NewExpectedStimulationsCount = ExpectedStimulationsCount + StimulatedNeighboursCount - 1,
            Notifier ! notification_processed,
            wait_for_inference_to_finish(NewExpectedStimulationsCount)
    end.


%% POTENTIAL OPTIMIZATION - async processing of node_stimulated messages
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


print_start(Event) ->
    % io:format("~p started...", [Event]),
    % erlang:monotonic_time(millisecond).
    ok.
    
print_end(StartTime) ->
    % io:format("finished [~f]~n", [(erlang:monotonic_time(millisecond) - StartTime) / 1000]).
    ok.
