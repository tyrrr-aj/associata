-module(vn).
-export([create_VN/8, 
        connect_VN/3, 
        connect_ON/2, 
        disconnect_ON/2,
        disconnect_VN/3,
        update_VNG_range/2, 
        update_VNG_to_ON_conn_count/2,
        stimulate/6, 
        get_excitation/2, 
        reset_excitation/1,
        wait_for_reset_excitation/1,
        get_neighbours/1,
        get_left_connected_vn/1,
        get_right_connected_vn/1,
        get_repr_value_and_vng_name/1,
        delete/1
        , report_stimulations_count/1
    ]).

-include("config.hrl").

-record(state, {vn_type, 
                vng, 
                vng_name, 
                vng_is_action, 
                vng_to_on_conn_count, 
                repr_value, 
                connected_vns, 
                connected_ons, 
                last_excitation, 
                curr_stimulation_id, 
                stimulated_neighs,
                vng_range, 
                agds, 
                global_cfg
                , dbg_stimulations_count=0
            }).



%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%% 

create_VN(RepresentedValue, categorical, VNGName, VNGIsAction, VNG, VNGtoONConnCount, AGDS, GlobalCfg) -> 
    spawn(fun() -> init(RepresentedValue, categorical, VNG, VNGName, VNGIsAction, VNGtoONConnCount, AGDS, GlobalCfg) end);

create_VN(RepresentedValue, VNGRange, VNGName, VNGIsAction, VNG, VNGtoONConnCount, AGDS, GlobalCfg) -> 
    spawn(fun() -> init(RepresentedValue, VNGRange, VNG, VNGName, VNGIsAction, VNGtoONConnCount, AGDS, GlobalCfg) end).


connect_VN(ThisVN, ConnectedVN, ConnectedVNValue) -> ThisVN ! {connect_VN, ConnectedVN, ConnectedVNValue}.


connect_ON(ThisVN, ON) -> ThisVN ! {connect_ON, ON}.


disconnect_ON(ThisVN, ON) -> ThisVN ! {disconnect_ON, ON}.


disconnect_VN(ThisVN, DisconnectedVN, ReplacementVN) -> ThisVN ! {disconnect_VN, DisconnectedVN, ReplacementVN}.


update_VNG_range(ThisVN, NewVNGRange) -> ThisVN ! {update_VNG_range, NewVNGRange}.


update_VNG_to_ON_conn_count(ThisVN, NewVNGtoONConnCount) -> ThisVN ! {update_VNG_to_ON_conn_count, NewVNGtoONConnCount}.


stimulate(ThisVN, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind) -> 
    ThisVN ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind}.


get_excitation(ThisVN, LastStimulationId) -> 
    ThisVN ! {get_excitation, self(), LastStimulationId},
    receive
        {last_excitation, Excitation} -> Excitation;
        {remove_killed_vn, ReprValue, VN} -> 
            vng:remove_killed_vn(self(), ReprValue, VN),
            none
    end.


reset_excitation(ThisVN) -> 
    ThisVN ! {reset_excitation, self()}.

wait_for_reset_excitation(ThisVN) ->
    receive
        {reset_excitation_finished, ThisVN} -> ok;
        {remove_killed_vn, ReprValue, VN} -> 
            vng:remove_killed_vn(self(), ReprValue, VN),
            ok
    end.


get_neighbours(ThisVN) -> ThisVN ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours;
        {remove_killed_vn, ReprValue, VN} -> 
            vng:remove_killed_vn(self(), ReprValue, VN),
            []
    end.


get_left_connected_vn(ThisVN) ->
    ThisVN ! {get_left_connected_vn, self()},
    receive
        {left_connected_vn, LeftConnectedVN} -> LeftConnectedVN
    end.


get_right_connected_vn(ThisVN) ->
    ThisVN ! {get_right_connected_vn, self()},
    receive
        {right_connected_vn, RightConnectedVN} -> RightConnectedVN
    end.


get_repr_value_and_vng_name(ThisVN) -> ThisVN ! {get_repr_value_and_vng_name, self()},
    receive
        {repr_value_and_vng_name, ReprValue, VNGName} -> {ReprValue, VNGName}
    end.


report_stimulations_count(ThisVN) -> 
    ThisVN ! {report_stimulations_count, self()},
    receive
        {stimulations_count_reported, ThisVN} -> ok;
        {remove_killed_vn, ReprValue, ThisVN} -> vng:remove_killed_vn(self(), ReprValue, ThisVN)
    end.


delete(ThisVN) -> ThisVN ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(RepresentedValue, categorical, VNG, VNGName, VNGIsAction, VNGtoONConnCount, AGDS, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=categorical, 
        vng=VNG, 
        vng_name=VNGName, 
        vng_is_action=VNGIsAction,
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue, 
        connected_vns=na, 
        connected_ons=[], 
        last_excitation=0.0, 
        curr_stimulation_id=none,
        stimulated_neighs=#{},
        vng_range=na, 
        agds=AGDS, 
        global_cfg=GlobalCfg
    });

init(RepresentedValue, VNGRange, VNG, VNGName, VNGIsAction, VNGtoONConnCount, AGDS, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=numeric, 
        vng=VNG, 
        vng_name=VNGName, 
        vng_is_action=VNGIsAction,
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue, 
        connected_vns={none, none}, 
        connected_ons=[], 
        last_excitation=0.0, 
        curr_stimulation_id=none,
        stimulated_neighs=#{},
        vng_range=VNGRange, 
        agds=AGDS, 
        global_cfg=GlobalCfg
    }).



process_events(#state{
        vn_type=VNType,
        vng=VNG,
        vng_name=VNGName,
        vng_is_action=VNGIsAction,
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue,
        connected_vns=ConnectedVNs, 
        connected_ons=ConnectedONs, 
        last_excitation=LastExcitation, 
        curr_stimulation_id=CurrStimulationId,
        stimulated_neighs=StimulatedNeighs,
        vng_range=VNGRange, 
        agds=AGDS, 
        global_cfg=#global_cfg{reporter=Reporter, timestep_ms=TimestepMs} = GlobalCfg
        , dbg_stimulations_count=DbgStimulationsCount
    } = State) -> 

    receive
        {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind} ->
            PassedStimulationId = case StimulationKind of
                {infere, InferenceId} -> InferenceId;
                {poison, _DeadlyDose, _MinimumAccumulatedDose, PoisoningId} -> PoisoningId
            end,

            dbg_counter:add_stimulations({vn, VNGName}, 1, PassedStimulationId, GlobalCfg#global_cfg.dbg_counter),

            CurrExcitation = case PassedStimulationId of
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            
            NewInferenceDepth = CurrInferenceDepth + 1,
            NewExcitation = CurrExcitation + Stimuli,
            
            % case StimulationKind of
            %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
            %     {poison, DeadlyDose} -> report:node_poisoned(self(), NewExcitation, DeadlyDose, Reporter)
            % end,
            
            if 
                NewInferenceDepth < MaxInferenceDepth ->
                    % timer:sleep(TimestepMs),

                    StimulatedNeighVNPidsAndValues = case VNType of
                        categorical -> [];
                        numeric -> [Neigh || Neigh <- tuple_to_list(ConnectedVNs), Neigh /= none, element(1, Neigh) /= Source]
                    end,
                    
                    StimulatedNeighONPids = lists:delete(Source, ConnectedONs);
                    
                true -> 
                    % case StimulationKind of
                    %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
                    %     {poison, DeadlyDose} -> report:node_poisoned(self(), NewExcitation, DeadlyDose, Reporter)
                    % end
                    StimulatedNeighVNPidsAndValues = [],
                    StimulatedNeighONPids = []
            end,

            StimulatedVNsAndONs = lists:map(fun({Pid, _Value}) -> Pid end, StimulatedNeighVNPidsAndValues) ++ StimulatedNeighONPids,
            NewStimulatedNeighs = case StimulatedVNsAndONs of
                [] -> 
                    Source ! {stimulation_finished, self(), CurrInferenceDepth}, % TODO: hide message in function, in common "agds_node" module
                    StimulatedNeighs;
                _ -> 
                    case StimulatedNeighs of
                        #{NewInferenceDepth := {AlreadyStimulatedNeighs, SourcesAtInfDepth}} -> 
                            #{NewInferenceDepth => {AlreadyStimulatedNeighs ++ StimulatedVNsAndONs, SourcesAtInfDepth ++ [Source]}};
                        _ -> 
                            StimulatedNeighs#{NewInferenceDepth => {StimulatedVNsAndONs, [Source]}}
                    end
            end,

            % agds:notify_node_stimulated(AGDS, length(StimulatedNeighVNPidsAndValues) + length(StimulatedNeighONPids)),

            lists:foreach(
                fun(VN) -> stimulate_neigh_vn(
                    VN, RepresentedValue, Stimuli, VNGRange, NewInferenceDepth, MaxInferenceDepth, StimulationKind
                ) end,
                StimulatedNeighVNPidsAndValues
            ),

            ONStimuli = Stimuli * weight_vn_to_on(ConnectedONs, VNGtoONConnCount),

            if 
                ONStimuli > 0.5 ->
                    lists:foreach(
                        fun(ON) -> on:stimulate(ON, self(), ONStimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind, VNGIsAction) end, 
                        StimulatedNeighONPids
                    );
                true -> 
                    lists:foreach(
                        fun(ON) -> self() ! {stimulation_finished, ON, NewInferenceDepth} end, 
                        StimulatedNeighONPids
                    )
            end,
            
            process_events(State#state{last_excitation=NewExcitation, curr_stimulation_id=PassedStimulationId, stimulated_neighs=NewStimulatedNeighs, dbg_stimulations_count=DbgStimulationsCount + 1});


        {stimulation_finished, StimulatedNode, InferenceDepth} ->
            % io:format("VN(~p,~p) received stimulation finished from ~p at depth ~p, current StimulatedNeighs: ~p~n", [VNGName, RepresentedValue, StimulatedNode, InferenceDepth, StimulatedNeighs]),
            NewStimulatedNeighs = case StimulatedNeighs of
                #{InferenceDepth := {[StimulatedNode], SourcesAtInfDepth}} -> 
                    lists:foreach(fun(Source) -> Source ! {stimulation_finished, self(), InferenceDepth - 1} end, SourcesAtInfDepth),
                    maps:remove(InferenceDepth, StimulatedNeighs);
                #{InferenceDepth := {StimulatedNeighsAtInfDepth, SourcesAtInfDepth}} -> 
                    StimulatedNeighs#{InferenceDepth => {lists:delete(StimulatedNode, StimulatedNeighsAtInfDepth), SourcesAtInfDepth}}
            end,
            process_events(State#state{stimulated_neighs=NewStimulatedNeighs});


        {connect_VN, NewConnectedVN, ConnectedVNValue} ->
            % io:format("VNGName: ~p  RepresentedValue: ~p  ConnectedVNValue: ~p~n", [VNGName, RepresentedValue, ConnectedVNValue]),
            if
                % no need to handle categorical VN case - connect_VN should never be called on such VN
                ConnectedVNValue < RepresentedValue -> 
                    {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
                    report_breaking_connection(LeftConnectedVN, NewConnectedVN, GlobalCfg),
                    process_events(State#state{connected_vns={{NewConnectedVN, ConnectedVNValue}, RightConnectedVN}});
                ConnectedVNValue > RepresentedValue ->
                    {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
                    report_breaking_connection(RightConnectedVN, NewConnectedVN, GlobalCfg),
                    process_events(State#state{connected_vns={LeftConnectedVN, {NewConnectedVN, ConnectedVNValue}}})
            end;


        {connect_ON, ON} -> 
            process_events(State#state{connected_ons=[ON | ConnectedONs]});


        {disconnect_ON, ON} ->
            NewConnectedONs = lists:delete(ON, ConnectedONs),
            vng:notify_VNG_to_ON_conn_count_decremented(VNG),
            
            case NewConnectedONs of
                [] -> 
                    report:node_killed(self(), Reporter),

                    case VNType of
                        categorical -> ok;
                        numeric ->
                            {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
                            case LeftConnectedVN of
                                none -> ok;
                                {LeftVN, _LeftReprValue} -> vn:disconnect_VN(LeftVN, self(), RightConnectedVN)
                            end,
                            case RightConnectedVN of
                                none -> ok;
                                {RightVN, _RightReprValue} -> vn:disconnect_VN(RightVN, self(), LeftConnectedVN)
                            end
                    end,

                    vng:remove_killed_vn(VNG, RepresentedValue, self()),
                    % dbg_counter:add_stimulations({vn, VNGName}, DbgStimulationsCount, GlobalCfg#global_cfg.dbg_counter),
                    zombie_wait_for_orphan_stimulations(StimulatedNeighs);

                _ -> process_events(State#state{connected_ons=NewConnectedONs})
            end;


        {disconnect_VN, DisconnectedVN, {ReplVN, _ReplVNValue} = ReplacementVN} ->
            NewConnectedVNs = case ConnectedVNs of
                {{DisconnectedVN, _} = OldLeftConnectedVN, RightConnectedVN} -> 
                    report_breaking_connection(OldLeftConnectedVN, ReplacementVN, GlobalCfg),
                    % I<3Ola; Nokia: connecting_people; I<3Ola
                    {ReplacementVN, RightConnectedVN};
                
                {LeftConnectedVN, {DisconnectedVN, _} = OldRightConnectedVN} -> 
                    report_breaking_connection(OldRightConnectedVN, ReplacementVN, GlobalCfg),
                    {LeftConnectedVN, ReplacementVN}
            end,

            report:connection_formed(self(), ReplVN, Reporter),
            process_events(State#state{connected_vns=NewConnectedVNs});


        {update_VNG_range, NewVNGRange} -> 
            process_events(State#state{vng_range=NewVNGRange});


        {update_VNG_to_ON_conn_count, NewVNGtoONConnCount} ->
            process_events(State#state{vng_to_on_conn_count=NewVNGtoONConnCount});


        {get_excitation, Asker, LastStimulationId} -> 
            Excitation = case LastStimulationId of
                CurrStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {last_excitation, Excitation},
            process_events(State);
        

        {reset_excitation, Asker} -> 
            % report:node_stimulated(self(), 0.0, Reporter),
            Asker ! {reset_excitation_finished, self()},
            process_events(State#state{last_excitation=0.0});


        {get_neighbours, Asker} -> 
            NeighbouringVNs = case VNType of
                categorical -> [];
                numeric -> 
                    % VNGName stored in state#{} should be the same as VNG returned by vn:get_repr_value_and_vng_name/1 for any neigh, as only VNs from the same VNG can be neighbours
                    NeighVNsValues = [vn:get_repr_value_and_vng_name(VN) || {VN, _VNValue} <- tuple_to_list(ConnectedVNs), VN /= none],
                    [{vn, NeighVNGName, NeighReprValue} || {NeighReprValue, NeighVNGName} <- NeighVNsValues]
            end,
            NeighbouringONs = [{on, on:get_index(ON)} || ON <- ConnectedONs],
            Asker ! {neighbours, NeighbouringVNs ++ NeighbouringONs},
            process_events(State);


        {get_left_connected_vn, Asker} ->
            {LeftConnectedVN, _RightConnectedVN} = ConnectedVNs,
            Asker ! {left_connected_vn, LeftConnectedVN},
            process_events(State);


        {get_right_connected_vn, Asker} ->
            {_LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
            Asker ! {right_connected_vn, RightConnectedVN},
            process_events(State);


        {get_repr_value_and_vng_name, Asker} ->
            Asker ! {repr_value_and_vng_name, RepresentedValue, VNGName},
            process_events(State);

        
        % {report_stimulations_count, Asker} ->
        %     dbg_counter:add_stimulations({vn, VNGName}, DbgStimulationsCount, GlobalCfg#global_cfg.dbg_counter),
        %     Asker ! {stimulations_count_reported, self()},
        %     process_events(State);

    
        delete -> ok

    end.


% stimulate_neigh_vn(none, _OwnReprValue, _ReceivedStimuli, _VNGRange, _NewInferenceDepth, _MaxInferenceDepth, _StimulationKind) -> noop;

stimulate_neigh_vn({TargetVN, TargetVNReprValue}, OwnReprValue, ReceivedStimuli, VNGRange, NewInferenceDepth, MaxInferenceDepth, StimulationKind) ->
    StimuliToPass = ReceivedStimuli * (VNGRange - abs(OwnReprValue - TargetVNReprValue)) / VNGRange,
    if
        StimuliToPass > 0.3 ->
            vn:stimulate(TargetVN, self(), StimuliToPass, NewInferenceDepth, MaxInferenceDepth, StimulationKind);
        true -> 
            self() ! {stimulation_finished, TargetVN, NewInferenceDepth}
    end.


weight_vn_to_on(ConnectedONs, VNGtoONConnCount) ->
    %% CLASSICAL WEIGHT
    % case length(ConnectedONs) > 0 of
    %     true ->  1 / length(ConnectedONs);
    %     false -> 0.0
    % end.
    
    %% MODIFIED WEIGHT
    if 
        VNGtoONConnCount == length(ConnectedONs) -> 1.0;
        true -> (VNGtoONConnCount - length(ConnectedONs)) / VNGtoONConnCount
    end.


report_breaking_connection(none, _NewConnectedVN, _GlobalCfg) -> ok;

report_breaking_connection({OldConnectedVN, _OldConnectedVNValue}, NewConnectedVN, _GlobalCfg) when OldConnectedVN == NewConnectedVN -> ok;

report_breaking_connection({OtherVN, _OtherVNValue}, _NewConnectedVN, #global_cfg{reporter=Reporter}) -> report:connection_broken(self(), OtherVN, Reporter).


% zombie_wait_for_orphan_stimulations(#{}) -> killed;

zombie_wait_for_orphan_stimulations(StimulatedNeighs) ->
    receive
        {stimulate, Source, _Stimuli, CurrInferenceDepth, _MaxInferenceDepth, _StimulationKind} ->
            % agds:notify_node_stimulated(AGDS, 0),
            Source ! {stimulation_finished, self(), CurrInferenceDepth},
            zombie_wait_for_orphan_stimulations(StimulatedNeighs);
        {stimulation_finished, StimulatedNode, InferenceDepth} -> 
            NewStimulatedNeighs = case StimulatedNeighs of
                #{InferenceDepth := {[StimulatedNode], SourcesAtInfDepth}} -> 
                    lists:foreach(fun(Source) -> Source ! {stimulation_finished, self(), InferenceDepth - 1} end, SourcesAtInfDepth),
                    maps:remove(InferenceDepth, StimulatedNeighs);
                #{InferenceDepth := {StimulatedNeighsAtInfDepth, SourcesAtInfDepth}} -> 
                    StimulatedNeighs#{InferenceDepth => {lists:delete(StimulatedNode, StimulatedNeighsAtInfDepth), SourcesAtInfDepth}}
            end,
            zombie_wait_for_orphan_stimulations(NewStimulatedNeighs)
    after 5000 -> killed
    end.
