-module(on).
-export([
    create_ON/3, 
    connect_VN/4, 
    disconnect_VN/2, 
    stimulate/4, 
    get_excitation/2, 
    get_neighbours/1,
    delete/1
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

create_ON(ONG, ONIndex, GlobalCfg) -> spawn(fun() -> init(ONG, ONIndex, GlobalCfg) end).


connect_VN(ON, VN, ReprValue, VNGName) -> ON ! {connect, VN, ReprValue, VNGName}.


disconnect_VN(ON, VN) -> ON ! {disconnect, VN}.


stimulate(ON, Stimulus, CurrDepth, StimulationSpec) -> 
    ON ! {stimulate, self(), Stimulus, CurrDepth, StimulationSpec}.


get_excitation(ON, LastStimulationId) -> 
    ON ! {get_excitation, self(), LastStimulationId},
    receive
        {excitation, Excitation} -> Excitation;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            none
    end.


get_neighbours(ON) -> ON ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours;
        {remove_killed_ON, ONIndex} ->
            ong:remove_killed_ON(self(), ONIndex),
            []
    end.


delete(ON) -> ON ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(ONG, ONIndex, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), on, ONIndex, ONG, Reporter),
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
                node_group_modes=NodeGroupModes, 
                min_passed_stimulus=MinPassedStimulus, 
                kind=StimulationKind,
                params=StimulationParams
            }=StimulationSpec
        } ->

            dbg_counter:add_stimulations(on, 1, StimulationId, GlobalCfg#global_cfg.dbg_counter),
            
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
                            process_events(State#state{last_stimulation_id=StimulationId, last_excitation=CurrExcitation + Stimulus});
                        _ ->
                            stimulation:respond_to_stimulation(Source, Stimulus * CurrExcitation),
                            process_events(State#state{last_stimulation_id=StimulationId})
                    end;

                CurrONGMode ->
                    EffectiveStimulus = amplify_stimulus_with_responsive_vns(Stimulus, NewDepth, ConnectedVNs, StimulationSpec),
                    NewExcitation = CurrExcitation + EffectiveStimulus,

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
                                    stimulation:send_stimulation_finished(Source, CurrDepth),
                                    StimulatedNeighs;
                                _ -> 
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
                            stimulation:send_stimulation_finished(Source, CurrDepth),
                            #{}
                    end,

                    case StimulationKind of
                        inference ->
                            % report:node_stimulated(self(), NewExcitation, Reporter),
                            process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId, stimulated_neighs=NewStimulatedNeighs});

                        poisoning -> 
                            MinAccumulatedDose = maps:get(min_accumulated_dose, StimulationParams),

                            NewAccPoisonLvl = if
                                LastExcitation >= MinAccumulatedDose -> AccPoisonLvl + EffectiveStimulus;
                                NewExcitation >= MinAccumulatedDose -> AccPoisonLvl + NewExcitation;
                                true -> AccPoisonLvl
                            end,

                            % report:node_poisoned(self(), NewAccPoisonLvl, NewExcitation, Stimulus, Source, Reporter),
                            DeadlyDose = maps:get(deadly_dose, StimulationParams),

                            if 
                                NewAccPoisonLvl >= DeadlyDose -> 
                                    report:node_killed(self(), Reporter),
                                    [vn:disconnect_ON(VN, self()) || VN <- maps:keys(ConnectedVNs)],
                                    ong:remove_killed_ON(ONG, ONIndex),
                                    zombie_wait_for_orphan_stimulations(NewStimulatedNeighs);
                                true ->
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
            % io:format(
            %     "~s    ON ~p: stimulation_finished received from ~p at depth: ~p (confirmation count: ~p)~nstimulated neighs: ~p~n", 
            %     [utils:get_timestamp_str(), self(), StimulatedNode, Depth, ConfirmationCount, StimulatedNeighs]
            % ),
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


        {connect, VN, ReprValue, VNGName} ->
            process_events(State#state{connected_vns=ConnectedVNs#{VN => {ReprValue, VNGName}}});


        {disconnect, VN} ->
            process_events(State#state{connected_vns=maps:remove(VN, ConnectedVNs)});


        {get_excitation, Asker, LastStimulationId} -> 
            Excitation = case LastStimulationId of 
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {excitation, Excitation},
            process_events(State);
        

        {get_neighbours, Asker} -> 
            Response = [{vn, VNGName, ReprValue} || {ReprValue, VNGName} <- maps:values(ConnectedVNs)],
            Asker ! {neighbours, Response},
            process_events(State);


        delete -> ok
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



zombie_wait_for_orphan_stimulations(StimulatedNeighs) ->
    receive
        {stimulate, Source, _Stimulus, Depth, _StimulationSpec} ->
            stimulation:send_stimulation_finished(Source, Depth),
            zombie_wait_for_orphan_stimulations(StimulatedNeighs);
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
            zombie_wait_for_orphan_stimulations(NewStimulatedNeighs)
    after 5000 -> killed
    end.
