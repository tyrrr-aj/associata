-module(vn).
-export([create_VN/6, 
        connect_VN/3, 
        connect_ON/2, 
        disconnect_ON/2,
        disconnect_VN/3,
        update_VNG_range/2, 
        update_VNG_to_ON_conn_count/2,
        stimulate/6, 
        get_excitation/1, 
        reset_excitation/1,
        get_neighbours/1,
        get_left_connected_vn/1,
        get_right_connected_vn/1,
        get_repr_value_and_vng_name/1,
        delete/1]).

-include("config.hrl").

-record(state, {vn_type, vng, vng_name, vng_to_on_conn_count, repr_value, connected_vns, connected_ons, last_excitation, vng_range, global_cfg}).



%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%% 

create_VN(RepresentedValue, categorical, VNGName, VNG, VNGtoONConnCount, GlobalCfg) -> 
    spawn(fun() -> init(RepresentedValue, categorical, VNG, VNGName, VNGtoONConnCount, GlobalCfg) end);

create_VN(RepresentedValue, VNGRange, VNGName, VNG, VNGtoONConnCount, GlobalCfg) -> 
    spawn(fun() -> init(RepresentedValue, VNGRange, VNG, VNGName, VNGtoONConnCount, GlobalCfg) end).


connect_VN(ThisVN, ConnectedVN, ConnectedVNValue) -> ThisVN ! {connect_VN, ConnectedVN, ConnectedVNValue}.


connect_ON(ThisVN, ON) -> ThisVN ! {connect_ON, ON}.


disconnect_ON(ThisVN, ON) -> ThisVN ! {disconnect_ON, ON}.


disconnect_VN(ThisVN, DisconnectedVN, ReplacementVN) -> ThisVN ! {disconnect_VN, DisconnectedVN, ReplacementVN}.


update_VNG_range(ThisVN, NewVNGRange) -> ThisVN ! {update_VNG_range, NewVNGRange}.


update_VNG_to_ON_conn_count(ThisVN, NewVNGtoONConnCount) -> ThisVN ! {update_VNG_to_ON_conn_count, NewVNGtoONConnCount}.


stimulate(ThisVN, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind) -> 
    ThisVN ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind}.


get_excitation(ThisVN) -> 
    ThisVN ! {get_excitation, self()},
    receive
        {last_excitation, Excitation} -> Excitation;
        {remove_killed_vn, ReprValue, VN} -> 
            vng:remove_killed_vn(self(), ReprValue, VN),
            none
    end.


reset_excitation(ThisVN) -> 
    ThisVN ! {reset_excitation, self()},
    receive
        reset_excitation_finished -> ok;
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


delete(ThisVN) -> ThisVN ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(RepresentedValue, categorical, VNG, VNGName, VNGtoONConnCount, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=categorical, 
        vng=VNG, 
        vng_name=VNGName, 
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue, 
        connected_vns=na, 
        connected_ons=[], 
        last_excitation=0.0, 
        vng_range=na, 
        global_cfg=GlobalCfg
    });

init(RepresentedValue, VNGRange, VNG, VNGName, VNGtoONConnCount, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=numeric, 
        vng=VNG, 
        vng_name=VNGName, 
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue, 
        connected_vns={none, none}, 
        connected_ons=[], 
        last_excitation=0.0, 
        vng_range=VNGRange, 
        global_cfg=GlobalCfg
    }).



process_events(#state{
        vn_type=VNType,
        vng=VNG,
        vng_name=VNGName,
        vng_to_on_conn_count=VNGtoONConnCount,
        repr_value=RepresentedValue,
        connected_vns=ConnectedVNs, 
        connected_ons=ConnectedONs, 
        last_excitation=LastExcitation, 
        vng_range=VNGRange, 
        global_cfg=#global_cfg{reporter=Reporter, timestep_ms=TimestepMs} = GlobalCfg
    } = State) -> 

    receive
        {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind} ->
            NewInferenceDepth = CurrInferenceDepth + 1,
            NewExcitation = LastExcitation + Stimuli,
            
            % case StimulationKind of
            %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
            %     {poison, DeadlyDose} -> report:node_poisoned(self(), NewExcitation, DeadlyDose, Reporter)
            % end,
            
            if 
                NewInferenceDepth < MaxInferenceDepth ->
                    timer:sleep(TimestepMs),

                    case VNType of
                        categorical -> ok;
                        numeric -> 
                            lists:foreach(
                                fun(VN) -> stimulate_neigh_vn(
                                    VN, RepresentedValue, Stimuli, VNGRange, NewInferenceDepth, MaxInferenceDepth, StimulationKind
                                ) end, 
                                lists:delete(Source, tuple_to_list(ConnectedVNs))
                            )
                    end,

                    ONStimuli = Stimuli * weight_vn_to_on(ConnectedONs, VNGtoONConnCount),
                    
                    lists:foreach(
                        fun(ON) -> on:stimulate(ON, self(), ONStimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind) end, 
                        lists:delete(Source, ConnectedONs)
                    );
                true -> 
                    % case StimulationKind of
                    %     infere -> report:node_stimulated(self(), NewExcitation, Reporter);
                    %     {poison, DeadlyDose} -> report:node_poisoned(self(), NewExcitation, DeadlyDose, Reporter)
                    % end
                    ok
            end,
        
            process_events(State#state{last_excitation=NewExcitation});


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
                    killed;

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


        {get_excitation, Asker} -> 
            Asker ! {last_excitation, LastExcitation},
            process_events(State);
        

        {reset_excitation, Asker} -> 
            % report:node_stimulated(self(), 0.0, Reporter),
            Asker ! reset_excitation_finished,
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

    
        delete -> ok

    end.


stimulate_neigh_vn(none, _OwnReprValue, _ReceivedStimuli, _VNGRange, _NewInferenceDepth, _MaxInferenceDepth, _StimulationKind) -> noop;

stimulate_neigh_vn({TargetVN, TargetVNReprValue}, OwnReprValue, ReceivedStimuli, VNGRange, NewInferenceDepth, MaxInferenceDepth, StimulationKind) ->
    StimuliToPass = ReceivedStimuli * (VNGRange - abs(OwnReprValue - TargetVNReprValue)) / VNGRange,
    vn:stimulate(TargetVN, self(), StimuliToPass, NewInferenceDepth, MaxInferenceDepth, StimulationKind).


weight_vn_to_on(ConnectedONs, VNGtoONConnCount) ->
    %% CLASSICAL WEIGHT
    % case length(ConnectedONs) > 0 of
    %     true ->  1 / length(ConnectedONs);
    %     false -> 0.0
    % end.
    
    %% MODIFIED WEIGHT
    (VNGtoONConnCount - length(ConnectedONs)) / VNGtoONConnCount.


report_breaking_connection(none, _NewConnectedVN, _GlobalCfg) -> ok;

report_breaking_connection({OldConnectedVN, _OldConnectedVNValue}, NewConnectedVN, _GlobalCfg) when OldConnectedVN == NewConnectedVN -> ok;

report_breaking_connection({OtherVN, _OtherVNValue}, _NewConnectedVN, #global_cfg{reporter=Reporter}) -> report:connection_broken(self(), OtherVN, Reporter).
