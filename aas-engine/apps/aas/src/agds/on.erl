-module(on).
-export([
    create_ON/4, 
    connect_VN/4, 
    disconnect_VN/2, 
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
    ong,                    % pid
    connected_vns,          % #{pid := {float | string, string}}
    last_excitation,        % float
    last_stimulation_id,    % int
    stimulated_neighs,      % #{int => {#{pid := int}, #{pid := int}}}
    acc_poison_lvl,         % float
    global_cfg             % #global_cfg
}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_ON(ONG, ONIndex, ExperimentStep, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, ExperimentStep, GlobalCfg) end).


connect_VN(ON, VN, ReprValue, VNGName) -> 
    ON ! {connect, self(), VN, ReprValue, VNGName},
    receive 
        {vn_connected, VN, ON} -> ok 
    end.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


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
        ong=ONG, 
        connected_vns=#{}, 
        last_excitation=0.0, 
        last_stimulation_id=none, 
        stimulated_neighs=#{},
        acc_poison_lvl=0.0,
        global_cfg=GlobalCfg
}).


process_events(#state{
    self_index=ONIndex, 
    ong=ONG, 
    connected_vns=ConnectedVNs, 
    last_excitation=LastExcitation, 
    last_stimulation_id=CurrStimulationId, 
    stimulated_neighs=StimulatedNeighs, 
    acc_poison_lvl=AccPoisonLvl,
    global_cfg=#global_cfg{reporter=Reporter}=GlobalCfg
} = State) -> 

    receive
        {
            stimulate, 
            Source, 
            Stimulus, 
            CurrDepth,
            #stim_spec{
                id=StimulationId, 
                experiment_step=ExperimentStep,
                name=StimulationName,
                write_to_log=WriteToLog,
                node_group_modes=NodeGroupModes, 
                min_passed_stimulus=MinPassedStimulus, 
                kind=StimulationKind,
                params=StimulationParams
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

                {responsive, excitation} ->
                    case Source of
                        ONG -> 
                            stimulation:send_stimulation_finished(Source, CurrDepth),
                            NewExcitation = CurrExcitation + Stimulus,
                            report:node_stimulated(WriteToLog, self(), Source, NewExcitation, Stimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),
                            process_events(State#state{last_stimulation_id=StimulationId, last_excitation=NewExcitation});
                        _ ->
                            stimulation:respond_to_stimulation(Source, Stimulus * CurrExcitation),
                            process_events(State#state{last_stimulation_id=StimulationId})
                    end;

                CurrONGMode ->
                    EffectiveStimulus = amplify_stimulus_with_responsive_vns(Stimulus, NewDepth, ConnectedVNs, StimulationSpec),
                    NewExcitation = CurrExcitation + EffectiveStimulus,

                    report:node_stimulated(WriteToLog, self(), Source, NewExcitation, Stimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),

                    NewStimulatedNeighs = case CurrONGMode of
                        transitive -> 
                            StimulatedVNs = if
                                EffectiveStimulus >= MinPassedStimulus -> [
                                    VN || {VN, {_ReprValue, VNGName}} <- maps:to_list(ConnectedVNs), 
                                                            not ng:is_responsive(VNGName, NodeGroupModes), 
                                                            not ng:is_transitive(VNGName, NodeGroupModes), 
                                                            VN =/= Source];
                                true -> []
                            end,

                            lists:foreach(fun(VN) -> vn:stimulate(VN, EffectiveStimulus, NewDepth, StimulationSpec) end, StimulatedVNs),
        
                            case StimulatedVNs of
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
                                                #{Source := SourceStimulationCount} -> SourcesAtDepth#{Source => SourceStimulationCount + 1};
                                                _ -> SourcesAtDepth#{Source => 1}
                                            end,
                                            StimulatedNeighs#{NewDepth => {NewStimulatedNeighsAtDepth, NewSourcesAtDepth}};
                                        
                                        _ -> StimulatedNeighs#{NewDepth => {lists:foldl(fun(VN, Acc) -> Acc#{VN => 1} end, #{}, StimulatedVNs), #{Source => 1}}}
                                    end
                            end;
                        
                        accumulative -> 
                            StimulatingNeighsFinished = true,
                            #{}
                    end,

                    case StimulationKind of
                        inference ->
                            if
                                StimulatingNeighsFinished -> stimulation:send_stimulation_finished(Source, CurrDepth);
                                true -> ok
                            end,
                            process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId, stimulated_neighs=NewStimulatedNeighs});

                        poisoning -> 
                            MinAccumulatedDose = maps:get(min_accumulated_dose, StimulationParams),

                            NewAccPoisonLvl = if
                                LastExcitation >= MinAccumulatedDose -> 
                                    report:node_poisoned(self(), AccPoisonLvl + EffectiveStimulus, ExperimentStep, Reporter),
                                    AccPoisonLvl + EffectiveStimulus;
                                
                                NewExcitation >= MinAccumulatedDose -> 
                                    report:node_poisoned(self(), AccPoisonLvl + NewExcitation, ExperimentStep, Reporter),
                                    AccPoisonLvl + NewExcitation;
                                
                                true -> AccPoisonLvl
                            end,

                            DeadlyDose = maps:get(deadly_dose, StimulationParams),

                            if 
                                NewAccPoisonLvl >= DeadlyDose -> 
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
                                        acc_poison_lvl=NewAccPoisonLvl, 
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


        {connect, Asker, VN, ReprValue, VNGName} ->
            Asker ! {vn_connected, VN, self()},
            process_events(State#state{connected_vns=ConnectedVNs#{VN => {ReprValue, VNGName}}});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=maps:remove(VN, ConnectedVNs)});


        {get_excitation, Asker, LastStimulationId} -> 
            Excitation = case LastStimulationId of 
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {excitation, self(), Excitation},
            process_events(State);
        

        {get_neighbours, Asker} -> 
            Response = [{vn, VNGName, ReprValue} || {ReprValue, VNGName} <- maps:values(ConnectedVNs)],
            Asker ! {neighbours, self(), Response},
            process_events(State);


        delete -> ok;


        reset_after_deadlock ->
            ONG ! {reset_after_deadlock_finished, self()},
            process_events(State#state{stimulated_neighs=#{}})
            
    end.



amplify_stimulus_with_responsive_vns(Stimulus, Depth, ConnectedVNs, #stim_spec{node_group_modes=NodeGroupModes}=StimulationSpec) ->
    ResponsiveNeighVNs = [VN || {VN, {_ReprValue, VNG}} <- maps:to_list(ConnectedVNs), ng:is_responsive(VNG, NodeGroupModes)],

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
