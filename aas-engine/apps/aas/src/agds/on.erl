-module(on).
-export([
    create_ON/4, 
    increment_occurances/1, 
    connect_VN/3, 
    disconnect_VN/2, 
    remove_outdated_connections/3,
    confirm_death_notification/2,
    stimulate/4, 
    get_excitation/2, 
    get_neighbours/1,
    delete/1,
    reset_after_deadlock/1
]).

-include("config.hrl").
-include("stimulation.hrl").

-record(state, {
    self_index,             % int
    n_occurances,           % int
    ong,                    % pid
    connected_vns,          % #{pid := {float | string, string}}
    last_excitation,        % float
    last_stimulation_id,    % int
    stimulated_neighs,      % #{int => {#{pid := int}, #{pid := int}}}
    acc_poison_lvls,        % #{string() := float()}  % per VNG accumulated poison level
    global_cfg             % #global_cfg
}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ON(ONG, ONIndex, ExperimentStep, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, ExperimentStep, GlobalCfg) end).


increment_occurances(ON) -> 
    ON ! {increment_occurances, self()},
    receive
        increment_occurances_finished -> ok
    end.


connect_VN(ON, VN, VNGName) -> 
    ON ! {connect, self(), VN, VNGName},
    receive 
        {vn_connected, VN, ON} -> ok 
    end.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


remove_outdated_connections(ON, AffectedVNGsAndCurrVNs, ExperimentStep) -> 
    ON ! {remove_outdated_connections, AffectedVNGsAndCurrVNs, ExperimentStep, self()},
    receive
        removal_finished -> ok
    end.


confirm_death_notification(ON, VN) -> ON ! {on_death_confirmed_by_vn, VN}.


stimulate(ON, Stimulus, CurrDepth, StimulationSpec) -> 
    ON ! {stimulate, self(), Stimulus, CurrDepth, StimulationSpec}.


get_excitation(ON, LastStimulationId) -> 
    ON ! {get_excitation, self(), LastStimulationId},
    receive
        {excitation, ON, Excitation} -> Excitation;
        {remove_killed_ON, ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            none
    end.


get_neighbours(ON) -> ON ! {get_neighbours, self()},
    receive
        {neighbours, ON, Neighbours} -> Neighbours;
        {remove_killed_ON, ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            []
    end.


delete(ON) -> ON ! delete.


reset_after_deadlock(ON) -> 
    ON ! reset_after_deadlock,
    receive
        {reset_after_deadlock_finished, ON} -> ok
    end.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(ONG, ONIndex, ExperimentStep, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), on, ONIndex, ONG, ExperimentStep, Reporter),
    process_events(#state{
        self_index=ONIndex, 
        n_occurances = 1, 
        ong=ONG, 
        connected_vns=#{}, 
        last_excitation=0.0, 
        last_stimulation_id=none, 
        stimulated_neighs=#{},
        acc_poison_lvls=#{},
        global_cfg=GlobalCfg
}).


process_events(#state{
    self_index=ONIndex, 
    n_occurances=NOccurances, 
    ong=ONG, 
    connected_vns=ConnectedVNs, 
    last_excitation=LastExcitation, 
    last_stimulation_id=CurrStimulationId, 
    stimulated_neighs=StimulatedNeighs, 
    acc_poison_lvls=AccPoisonLvls,
    global_cfg=#global_cfg{reporter=Reporter}=_GlobalCfg
} = State) -> 

    receive
        {
            stimulate, 
            Source, 
            Stimulus, 
            CurrDepth,
            #stim_spec{
                stimulation_id=StimulationId, 
                experiment_step=ExperimentStep,
                stimulation_name=StimulationName,
                should_write_to_log=WriteToLog,
                stimulation_kind=StimulationKind,
                node_group_modes=NodeGroupModes,
                poisoning_params=StimulationParams
            }=StimulationSpec
        } ->
            NewDepth = CurrDepth + 1,
            CurrExcitation = case StimulationId of 
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,

            case maps:get("ong", NodeGroupModes) of
                passive -> 
                    stimulation:send_stimulation_finished(Source, CurrDepth),
                    process_events(State);

                CurrONGMode ->
                    EffectiveStimulus = get_effective_stimulus(Source, Stimulus, NewDepth, ConnectedVNs, AccPoisonLvls, NOccurances, StimulationSpec),
                    NewExcitation = CurrExcitation + EffectiveStimulus,
 
                    % io:format("[Step ~p | ~p] ON ~p: Received stimulus ~p from ~p at depth ~p. Effective stimulus: ~p. New excitation: ~p~n", [ExperimentStep, StimulationName, ONIndex, Stimulus, Source, CurrDepth, EffectiveStimulus, NewExcitation]),

                    report:node_stimulated(WriteToLog, self(), Source, NewExcitation, EffectiveStimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),

                    {NewStimulatedNeighs, StimulatingNeighsFinished} = if
                        CurrONGMode =:= transitive -> 
                            stimulate_vns(ConnectedVNs, StimulatedNeighs, EffectiveStimulus, NewDepth, Source, StimulationSpec);

                        CurrONGMode =:= accumulative andalso StimulationKind =:= poisoning andalso Source =:= ONG -> 
                            stimulate_vns(ConnectedVNs, StimulatedNeighs, EffectiveStimulus, NewDepth, Source, StimulationSpec);

                        true ->
                            {#{}, true}
                    end,

                    case StimulationKind of
                        inference ->
                            if
                                StimulatingNeighsFinished -> stimulation:send_stimulation_finished(Source, CurrDepth);
                                true -> ok
                            end,
                            process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId, stimulated_neighs=NewStimulatedNeighs});

                        poisoning -> 
                            NewAccPoisonLvls = accumulate_poison(
                                ONG,
                                AccPoisonLvls,
                                LastExcitation,
                                NewExcitation,
                                EffectiveStimulus,
                                Source,
                                ConnectedVNs,
                                ExperimentStep,
                                Reporter,
                                StimulationParams
                            ),
                            DeadlyDose = maps:get(deadly_dose, StimulationParams),
                            AllVNGsDeadly = deadly_poison_reached_for_all_vngs(NewAccPoisonLvls, ConnectedVNs, DeadlyDose),

                            if 
                                AllVNGsDeadly -> 
                                    report:node_killed(self(), ExperimentStep, Reporter),
                                    [vn:disconnect_ON(VN, self(), ExperimentStep) || VN <- maps:keys(ConnectedVNs)],
                                    ong:remove_killed_ON(ONG, ONIndex),
                                    ZombieStimulatedNeighs = case StimulatingNeighsFinished of
                                        true -> NewStimulatedNeighs#{NewDepth => {#{}, #{Source => 1}}};
                                        false -> NewStimulatedNeighs
                                    end,
                                    zombie_wait_for_orhpan_messages(ZombieStimulatedNeighs, NewDepth, maps:keys(ConnectedVNs));
                                true ->
                                    if 
                                        StimulatingNeighsFinished -> stimulation:send_stimulation_finished(Source, CurrDepth);
                                        true -> ok
                                    end,
                                    process_events(State#state{
                                        last_stimulation_id=StimulationId,
                                        last_excitation=NewExcitation,
                                        acc_poison_lvls=NewAccPoisonLvls, 
                                        stimulated_neighs=NewStimulatedNeighs
                                    })
                            end
                    end
            end;

        
        {stimulation_finished, StimulatedNode, Depth, ConfirmationCount} ->
            NewStimulatedNeighs = case StimulatedNeighs of
                #{Depth := {#{StimulatedNode := ConfirmationCount}=NeighsAtDepth, SourcesAtDepth}} -> 
                    NewNeighsAtDepth = maps:remove(StimulatedNode, NeighsAtDepth),
                    if
                        map_size(NewNeighsAtDepth) == 0 ->
                            maps:foreach(fun(Source, StimCount) -> stimulation:send_stimulation_finished(Source, Depth - 1, StimCount) end, SourcesAtDepth),
                            maps:remove(Depth, StimulatedNeighs);
                        true ->
                            StimulatedNeighs#{Depth => {NewNeighsAtDepth, SourcesAtDepth}}
                    end;

                #{Depth := {#{StimulatedNode := StimulationCount}=NeighsAtDepth, SourcesAtDepth}} -> 
                    StimulatedNeighs#{Depth => {NeighsAtDepth#{StimulatedNode => StimulationCount - ConfirmationCount}, SourcesAtDepth}}
            end,
            process_events(State#state{stimulated_neighs=NewStimulatedNeighs});


        {increment_occurances, Asker} ->
            Asker ! increment_occurances_finished,
            process_events(State#state{n_occurances=NOccurances + 1});


        {connect, Asker, VN, VNGName} ->
            % Disconnect any prevoiously connected VN from the same VNG
            % ExistingVNsForVNG = [ExistingVN || {ExistingVN, {_ExistingReprValue, ExistingVNGName}} <- maps:to_list(ConnectedVNs), ExistingVNGName =:= VNGName],
            % lists:foreach(fun(ExistingVN) ->
            %     vn:disconnect_ON(ExistingVN, self(), 0) %% TODO: Handle ExperimentStep properly
            % end, ExistingVNsForVNG),

            Asker ! {vn_connected, VN, self()},
            process_events(State#state{connected_vns=ConnectedVNs#{VN => VNGName}});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=maps:remove(VN, ConnectedVNs)});


        {remove_outdated_connections, AffectedVNGsAndCurrVNs, ExperimentStep, Asker} -> 
            NewConnectedVNs = maps:filter(
                fun(VN, VNGName) -> 
                    case maps:get(VNGName, AffectedVNGsAndCurrVNs, none) of
                        none -> true;
                        VN -> true;
                        _ -> false
                    end
                end,
                ConnectedVNs),

            RemovedVNs = maps:keys(ConnectedVNs) -- maps:keys(NewConnectedVNs),
            % io:format("ON ~p: Removing VNs ~p replaced by ~p~n", [ONIndex, lists:map(fun(VN) -> maps:get(VN, ConnectedVNs) end, RemovedVNs), AffectedVNGsAndCurrVNs]),
            lists:foreach(fun(VN) -> vn:disconnect_ON(VN, self(), ExperimentStep) end, RemovedVNs),
            
            Asker ! removal_finished,
            process_events(State#state{connected_vns=NewConnectedVNs});


        {get_excitation, Asker, LastStimulationId} -> 
            Excitation = case LastStimulationId of 
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {excitation, self(), Excitation},
            process_events(State);
        

        {get_neighbours, Asker} -> 
            Response = [{vn, VNGName, vn:get_repr_value(VN), VN} || {VN, VNGName} <- maps:to_list(ConnectedVNs)],
            Asker ! {neighbours, self(), Response},
            process_events(State);


        delete -> ok;


        reset_after_deadlock ->
            ONG ! {reset_after_deadlock_finished, self()},
            process_events(State#state{stimulated_neighs=#{}})
            
    end.



weight_poisoning(PoisonLvl) -> PoisonLvl.


get_effective_stimulus(Source, Stimulus, NewDepth, ConnectedVNs, AccPoisonLvls, NOccurances, 
    #stim_spec{stimulation_kind=StimulationKind, vn_to_on_weight_mode=VNToONWeightMode}=StimulationSpec
) ->
    ReceivedStimulusWeighted = case should_apply_weight_vn_to_on(VNToONWeightMode, ConnectedVNs, Source) of
        true -> apply_local_part_of_weight_vn_to_on(Stimulus, NOccurances);
        false -> Stimulus
    end,
    AmplifiedStimulus = amplify_stimulus_with_responsive_vns(ReceivedStimulusWeighted, NewDepth, ConnectedVNs, StimulationSpec),
    PoisonLvl = case maps:get(Source, ConnectedVNs, undefined) of
        undefined -> 0.0;
        VNGName -> maps:get(VNGName, AccPoisonLvls, 0.0)
    end,
    OutputStimulusWeighted = case StimulationKind of
        poisoning -> AmplifiedStimulus;
        _ -> AmplifiedStimulus - weight_poisoning(PoisonLvl)
    end,
    if 
        OutputStimulusWeighted < 0.0 -> 0.0;
        true -> OutputStimulusWeighted
    end.


apply_local_part_of_weight_vn_to_on(Stimulus, NOccurances) ->
    Stimulus * NOccurances.


should_apply_weight_vn_to_on(WeightMode, ConnectedVNs, Source) -> 
    is_vn_to_on_weight_proportional_to_on_occurances(WeightMode) andalso is_vn(Source, ConnectedVNs).


is_vn_to_on_weight_proportional_to_on_occurances(VNToONWeightMode) ->
    VNToONWeightMode =:= rate_of_occurance.

is_vn(Source, ConnectedVNs) -> maps:is_key(Source, ConnectedVNs).


accumulate_poison(ONG, CurrAccPoisonLvls, LastExcitation, NewExcitation, EffectiveStimulus, Source, ConnectedVNs, ExperimentStep, Reporter, StimulationParams) ->
    MinAccumulatedDose = maps:get(min_accumulated_dose, StimulationParams),

    %% Only accumulate if source is a VN (not ONG) and belongs to a VNG
    case maps:is_key(Source, ConnectedVNs) of
        false -> CurrAccPoisonLvls;  % Source not a VN or not connected
        true ->
            VNGName = maps:get(Source, ConnectedVNs),
            CurrForVNG = maps:get(VNGName, CurrAccPoisonLvls, 0.0),
            NewForVNG = if
                Source =:= ONG -> CurrForVNG;  % safeguard, though maps:is_key(Source, ConnectedVNs) false for ONG
                LastExcitation >= MinAccumulatedDose -> 
                    report:node_poisoned(self(), CurrForVNG + EffectiveStimulus, ExperimentStep, Reporter),
                    CurrForVNG + EffectiveStimulus;
                NewExcitation >= MinAccumulatedDose -> 
                    report:node_poisoned(self(), CurrForVNG + NewExcitation, ExperimentStep, Reporter),
                    CurrForVNG + NewExcitation;
                true -> CurrForVNG
            end,
            CurrAccPoisonLvls#{VNGName => NewForVNG}
    end.

deadly_poison_reached_for_all_vngs(AccPoisonLvls, ConnectedVNs, DeadlyDose) ->
    VNGNames = lists:usort([VNGName || VNGName <- maps:values(ConnectedVNs), VNGName =/= "action" andalso VNGName =/= "value" ]),
    lists:all(fun(VNGName) -> maps:get(VNGName, AccPoisonLvls, 0.0) >= DeadlyDose end, VNGNames).


stimulate_vns(
    ConnectedVNs, 
    StimulatedNeighs, 
    EffectiveStimulus,
    NewDepth,
    StimulationSource, 
    #stim_spec{
        node_group_modes=NodeGroupModes, 
        min_passed_stimulus=MinPassedStimulus, 
        stimulation_kind=StimulationKind
    }=StimulationSpec
) ->
    StimulatedVNs = if
        EffectiveStimulus >= MinPassedStimulus -> [
                VN || {VN, VNGName} <- maps:to_list(ConnectedVNs), 
                                                    ng:is_accumulative(VNGName, NodeGroupModes) orelse (ng:is_transitive(VNGName, NodeGroupModes) andalso StimulationKind =:= poisoning)
            ];
        true -> []
    end,

    lists:foreach(fun(VN) -> vn:stimulate(VN, EffectiveStimulus, NewDepth, StimulationSpec) end, StimulatedVNs),

    NewStimulatedNeighs = case StimulatedVNs of
        [] -> 
            StimulatingNeighsFinished = true,
            StimulatedNeighs;
        _ -> 
            StimulatingNeighsFinished = false,
            case StimulatedNeighs of
                #{NewDepth := {StimulatedNeighsAtDepth, SourcesAtDepth}} -> 
                    NewStimulatedNeighsAtDepth = lists:foldl(
                        fun(VN, Acc) ->
                            case Acc of
                                #{VN := NeighStimulationCount} -> Acc#{VN => NeighStimulationCount + 1};
                                _ -> Acc#{VN => 1}
                            end
                        end,
                        StimulatedNeighsAtDepth,
                        StimulatedVNs
                    ),
                    NewSourcesAtDepth = case SourcesAtDepth of
                        #{StimulationSource := SourceStimulationCount} -> SourcesAtDepth#{StimulationSource => SourceStimulationCount + 1};
                        _ -> SourcesAtDepth#{StimulationSource => 1}
                    end,
                    StimulatedNeighs#{NewDepth => {NewStimulatedNeighsAtDepth, NewSourcesAtDepth}};

                _ -> StimulatedNeighs#{NewDepth => {lists:foldl(fun(VN, Acc) -> Acc#{VN => 1} end, #{}, StimulatedVNs), #{StimulationSource => 1}}}
            end
    end,

    {NewStimulatedNeighs, StimulatingNeighsFinished}.



amplify_stimulus_with_responsive_vns(Stimulus, Depth, ConnectedVNs, #stim_spec{node_group_modes=NodeGroupModes}=StimulationSpec) ->
    ResponsiveNeighVNs = [VN || {VN, VNG} <- maps:to_list(ConnectedVNs), ng:is_responsive(VNG, NodeGroupModes)],

    case ResponsiveNeighVNs of
        [] -> Stimulus;
        _ ->
            lists:foreach(fun(VN) -> vn:stimulate(VN, Stimulus, Depth, StimulationSpec) end, ResponsiveNeighVNs),
            lists:foldl(
                fun(_VN, Acc) -> 
                    receive 
                        {stimulation_response, ResStimulus} -> Acc + ResStimulus
                    end
                end, 
                0.0, 
                ResponsiveNeighVNs
            )
    end.


zombie_wait_for_orhpan_messages(StimulatedNeighs, DiedAtDepth, VNsNotifiedOfDeath) ->
    receive
        {stimulate, Source, _Stimulus, Depth, _StimulationSpec} ->
            stimulation:send_stimulation_finished(Source, Depth),
            zombie_wait_for_orhpan_messages(StimulatedNeighs, DiedAtDepth, VNsNotifiedOfDeath);

        {stimulation_finished, StimulatedNode, Depth, ConfirmationCount} -> 
            NewStimulatedNeighs = case StimulatedNeighs of
                #{Depth := {#{StimulatedNode := ConfirmationCount}=NeighsAtDepth, SourcesAtDepth}} -> 
                    NewNeighsAtDepth = maps:remove(StimulatedNode, NeighsAtDepth),
                    if
                        map_size(NewNeighsAtDepth) == 0 andalso (Depth /= DiedAtDepth orelse VNsNotifiedOfDeath =:= []) ->
                            maps:foreach(fun(Source, StimCount) -> stimulation:send_stimulation_finished(Source, Depth - 1, StimCount) end, SourcesAtDepth),
                            maps:remove(Depth, StimulatedNeighs);
                        true ->
                            StimulatedNeighs#{Depth => {NewNeighsAtDepth, SourcesAtDepth}}
                    end;

                #{Depth := {#{StimulatedNode := StimulationCount}=NeighsAtDepth, SourcesAtDepth}} -> 
                    StimulatedNeighs#{Depth => {NeighsAtDepth#{StimulatedNode => StimulationCount - ConfirmationCount}, SourcesAtDepth}}
            end,
            zombie_wait_for_orhpan_messages(NewStimulatedNeighs, DiedAtDepth, VNsNotifiedOfDeath);

        {on_death_confirmed_by_vn, VN} ->
            NewVNsNotifiedOfDeath = lists:delete(VN, VNsNotifiedOfDeath),
            
            NewStimulatedNeighs = case StimulatedNeighs of
                #{DiedAtDepth := {NeighsStimulatedAtDeathDepth, SourcesAtDeathDepth}} ->
                    if
                        map_size(NeighsStimulatedAtDeathDepth) == 0 andalso NewVNsNotifiedOfDeath =:= [] -> 
                            maps:foreach(fun(Source, StimCount) -> stimulation:send_stimulation_finished(Source, DiedAtDepth - 1, StimCount) end, SourcesAtDeathDepth),
                            maps:remove(DiedAtDepth, StimulatedNeighs);
                        true ->
                            StimulatedNeighs
                    end;
                _ -> StimulatedNeighs
            end,
                
            zombie_wait_for_orhpan_messages(NewStimulatedNeighs, DiedAtDepth, NewVNsNotifiedOfDeath)

    after 5000 -> killed
    end.
