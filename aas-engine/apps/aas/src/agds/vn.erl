-module(vn).
-export([create_VN/5, 
        connect_VN/3, 
        connect_ON/2, 
        disconnect_ON/2,
        disconnect_VN/3,
        update_VNG_range/2, 
        stimulate/6, 
        get_excitation/1, 
        reset_excitation/1,
        get_neighbours/1,
        get_left_connected_vn/1,
        get_right_connected_vn/1,
        get_repr_value_and_vng_name/1,
        delete/1]).

-include("config.hrl").

-record(state, {vn_type, vng, vng_name, repr_value, connected_vns, connected_ons, last_excitation, vng_range, global_cfg}).



%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_VN(RepresentedValue, categorical, VNGName, VNG, GlobalCfg) -> spawn(fun() -> init(RepresentedValue, categorical, VNG, VNGName, GlobalCfg) end);

create_VN(RepresentedValue, VNGRange, VNGName, VNG, GlobalCfg) -> spawn(fun() -> init(RepresentedValue, VNGRange, VNG, VNGName, GlobalCfg) end).


connect_VN(ThisVN, ConnectedVN, ConnectedVNValue) -> ThisVN ! {connect_VN, ConnectedVN, ConnectedVNValue}.


connect_ON(ThisVN, ON) -> ThisVN ! {connect_ON, ON}.


disconnect_ON(ThisVN, ON) -> ThisVN ! {disconnect_ON, ON}.


disconnect_VN(ThisVN, DisconnectedVN, ReplacementVN) -> ThisVN ! {disconnect_VN, DisconnectedVN, ReplacementVN}.


update_VNG_range(ThisVN, NewVNGRange) -> ThisVN ! {update_VNG_range, NewVNGRange}.


stimulate(ThisVN, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind) -> 
    ThisVN ! {stimulate, Source, Stimuli, CurrInferenceDepth, MaxInferenceDepth, StimulationKind}.


get_excitation(ThisVN) -> 
    ThisVN ! {get_excitation, self()},
    receive
        {last_excitation, Excitation} -> Excitation
    end.


reset_excitation(ThisVN) -> 
    ThisVN ! {reset_excitation, self()},
    receive
        reset_excitation_finished -> ok
    end.


get_neighbours(ThisVN) -> ThisVN ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours
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

init(RepresentedValue, categorical, VNG, VNGName, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=categorical, 
        vng=VNG, 
        vng_name=VNGName, 
        repr_value=RepresentedValue, 
        connected_vns=na, 
        connected_ons=[], 
        last_excitation=0.0, 
        vng_range=na, 
        global_cfg=GlobalCfg
    });

init(RepresentedValue, VNGRange, VNG, VNGName, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
    process_events(#state{
        vn_type=numeric, 
        vng=VNG, 
        vng_name=VNGName, 
        repr_value=RepresentedValue, 
        connected_vns={none, none}, 
        connected_ons=[], 
        last_excitation=0.0, 
        vng_range=VNGRange, 
        global_cfg=GlobalCfg
    }).



process_events(#state{
        vn_type=VNType,
        vng_name=VNGName,
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

                    ONStimuli = case length(ConnectedONs) > 0 of
                        true ->  Stimuli / length(ConnectedONs);
                        false -> 0.0
                    end,
                    
                    lists:foreach(
                        fun(ON) -> on:stimulate(ON, self(), ONStimuli, NewInferenceDepth, MaxInferenceDepth, StimulationKind) end, 
                        lists:delete(Source, ConnectedONs)
                    );
                true -> 
                    case StimulationKind of
                        infere -> report:node_stimulated(self(), NewExcitation, Reporter);
                        {poison, DeadlyDose} -> report:node_poisoned(self(), NewExcitation, DeadlyDose, Reporter)
                    end
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
            process_events(State#state{connected_ons=NewConnectedONs});
            % %%% REMOVING ORHPANED VNs - a good concept, but AVB+ trees do not support deleting...
            % case NewConnectedONs of
            %     [] -> 
            %         report:node_killed(self(), Reporter),

            %         {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
            %         case LeftConnectedVN of
            %             none -> ok;
            %             {VN, _ReprValue} -> vn:disconnect_VN(VN, RightConnectedVN)
            %         end
            %         case RightConnectedVN of
            %             none -> ok;
            %             {VN, _ReprValue} -> vn:disconnect_VN(VN, LeftConnectedVN)
            %         end,

            %         vng:remove_killed_vn(VNG, self()),
            %         killed;

            %     _ -> process_events(State#state{connected_ons=NewConnectedONs})
            % end;


        {disconnect_VN, DisconnectedVN, ReplacementVN} ->
            NewConnectedVNs = case ConnectedVNs of
                {{DisconnectedVN, _}, RightConnectedVN} -> {ReplacementVN, RightConnectedVN};
                {LeftConnectedVN, {DisconnectedVN, _}} -> {LeftConnectedVN, ReplacementVN}
            end,
            process_events(State#state{connected_vns=NewConnectedVNs});


        {update_VNG_range, NewVNGRange} -> 
            process_events(State#state{vng_range=NewVNGRange});


        {get_excitation, Asker} -> 
            Asker ! {last_excitation, LastExcitation},
            process_events(State);
        

        {reset_excitation, Asker} -> 
            report:node_stimulated(self(), 0.0, Reporter),
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



report_breaking_connection(none, _NewConnectedVN, _GlobalCfg) -> ok;

report_breaking_connection({OldConnectedVN, _OldConnectedVNValue}, NewConnectedVN, _GlobalCfg) when OldConnectedVN == NewConnectedVN -> ok;

report_breaking_connection({OtherVN, _OtherVNValue}, _NewConnectedVN, #global_cfg{reporter=Reporter}) -> report:connection_broken(self(), OtherVN, Reporter).
