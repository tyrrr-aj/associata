-module(vn).
-export([create_VN/9, 
        connect_VN/4, 
        connect_ON/3, 
        disconnect_ON/3,
        disconnect_VN/6,
        confirm_vn_disconnected/3,
        update_VNG_range/3, 
        update_VNG_to_ON_conn_count/2,
        stimulate/4, 
        get_excitation/2,
        get_neighbours/1,
        get_neigh_vns/1,
        delete/1,
        reset_after_deadlock/1
    ]).

-include("config.hrl").
-include("stimulation.hrl").

-record(state, {vn_type,                % categorical | numerical
                vng,                    % pid
                vng_name,               % string
                vng_to_on_conn_count,   % integer
                repr_value,             % any
                connected_vns,          % {{pid, float} | none, {pid, float} | none} | na
                connected_ons,          % #{pid := integer}
                last_excitation,        % float
                last_stimulation_id,    % none | integer
                stimulated_neighs,      % #{integer => {#{pid := int}, #{pid := int}}}
                vng_min_value,          % float
                vng_max_value,          % float
                global_cfg              % #global_cfg
            }).



%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%% 

create_VN(categorical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, ExperimentStep, GlobalCfg) -> 
    spawn(fun() -> init(categorical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, ExperimentStep, GlobalCfg) end);

create_VN(numerical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, ExperimentStep, GlobalCfg) -> 
    spawn(fun() -> init(numerical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, ExperimentStep, GlobalCfg) end).


connect_VN(ThisVN, ConnectedVN, ConnectedVNValue, ExperimentStep) -> 
    ThisVN ! {connect_VN, self(), ConnectedVN, ConnectedVNValue, ExperimentStep},
    receive
        {vn_connected, ThisVN, ConnectedVN} -> ok;
        {remove_killed_vn, ReprValue, ThisVN} -> vng:remove_killed_vn(self(), ReprValue, ThisVN)
    end.



connect_ON(ThisVN, ON, ONIndex) -> 
    ThisVN ! {connect_ON, self(), ON, ONIndex},
    receive
        {on_connected, ThisVN, ON} -> ok;
        {remove_killed_vn, ReprValue, ThisVN} -> vng:remove_killed_vn(self(), ReprValue, ThisVN)
    end.


disconnect_ON(ThisVN, ON, ExperimentStep) -> ThisVN ! {disconnect_ON, ON, ExperimentStep}.

% Direction - of information flow, i.e. from DisconnectedVN to ThisVN
disconnect_VN(ThisVN, Direction, DisconnectedVN, ReplacementVN, IntermediateZombies, ExperimentStep) -> ThisVN ! {disconnect_VN, Direction, DisconnectedVN, ReplacementVN, IntermediateZombies, ExperimentStep}.

confirm_vn_disconnected(ThisVN, VN, IntermediateZombies) -> ThisVN ! {confirm_vn_disconnected, VN, IntermediateZombies}.


update_VNG_range(ThisVN, NewVNGMinValue, NewVNGMaxValue) -> ThisVN ! {update_VNG_range, NewVNGMinValue, NewVNGMaxValue}.


update_VNG_to_ON_conn_count(ThisVN, NewEntireVNGConnCount) -> ThisVN ! {update_VNG_to_ON_conn_count, NewEntireVNGConnCount}.


stimulate(ThisVN, Stimulus, CurrDepth, StimulationSpec) -> 
    ThisVN ! {stimulate, self(), Stimulus, CurrDepth, StimulationSpec}.


get_excitation(ThisVN, LastStimulationId) -> 
    ThisVN ! {get_excitation, self(), LastStimulationId},
    receive
        {last_excitation, ThisVN, Excitation} -> Excitation;
        {remove_killed_vn, ReprValue, ThisVN} -> 
            vng:remove_killed_vn(self(), ReprValue, ThisVN),
            none
    end.


get_neighbours(ThisVN) -> 
    ThisVN ! {get_neighbours, self()},
    receive
        {neighbours, ThisVN, Neighbours} -> Neighbours;
        {remove_killed_vn, ReprValue, ThisVN} -> 
            vng:remove_killed_vn(self(), ReprValue, ThisVN),
            []
    end.


get_neigh_vns(ThisVN) ->
    ThisVN ! {get_neigh_vns, self()},
    receive
        {neigh_vns, ThisVN, Neighbours} -> Neighbours;
        {remove_killed_vn, ReprValue, ThisVN} -> 
            vng:remove_killed_vn(self(), ReprValue, ThisVN),
            {none, none}
    end.


delete(ThisVN) -> ThisVN ! delete.


reset_after_deadlock(ThisVN) -> 
    ThisVN ! reset_after_deadlock,
    receive
        {reset_after_deadlock_finished, ThisVN} -> ok
    end.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(VNType, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, ExperimentStep, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, ExperimentStep, Reporter),
    process_events(#state{
        vn_type=VNType, 
        vng=VNG, 
        vng_name=VNGName, 
        vng_to_on_conn_count=EntireVNGConnCount,
        repr_value=RepresentedValue, 
        connected_vns=case VNType of categorical -> na; numerical -> {none, none} end,
        connected_ons=#{}, 
        last_excitation=0.0, 
        last_stimulation_id=none,
        stimulated_neighs=#{},
        vng_min_value=VNGMinValue,
        vng_max_value=VNGMaxValue,
        global_cfg=GlobalCfg
    }).


process_events(#state{
        vn_type=VNType,
        vng=VNG,
        vng_name=VNGName,
        vng_to_on_conn_count=EntireVNGConnCount,
        repr_value=RepresentedValue,
        connected_vns=ConnectedVNs, 
        connected_ons=ConnectedONs, 
        last_excitation=LastExcitation, 
        last_stimulation_id=LastStimulationId,
        stimulated_neighs=StimulatedNeighs,
        vng_min_value=VNGMinValue,
        vng_max_value=VNGMaxValue,
        global_cfg=#global_cfg{reporter=Reporter} = GlobalCfg
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
                kind=StimulationKind, 
                node_group_modes=NodeGroupModes,
                min_passed_stimulus=MinPassedStimulus
            }=StimulationSpec
        } ->
            dbg_counter:add_stimulations({vn, VNGName}, 1, StimulationId, GlobalCfg#global_cfg.dbg_counter),
            
            CurrExcitation = case StimulationId of
                LastStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            
            NewDepth = CurrDepth + 1,
            NewExcitation = CurrExcitation + Stimulus,  % This line assumes that ONG CANNOT work in responsive mode!

            case maps:get(VNGName, NodeGroupModes) of
                passive -> 
                    stimulation:send_stimulation_finished(Source, CurrDepth),
                    process_events(State#state{last_stimulation_id=StimulationId});

                {responsive, AmplifingFactor} -> 
                    case Source of
                        VNG ->
                            stimulation:send_stimulation_finished(Source, CurrDepth),
                            report:node_stimulated(WriteToLog, self(), Source, NewExcitation, Stimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),
                            process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId});
                        _ ->
                            ResStimulus = case AmplifingFactor of
                                excitation ->
                                    Stimulus * CurrExcitation;
                                value ->
                                    Stimulus * if 
                                        VNGMaxValue - VNGMinValue == 0.0 -> 1.0;
                                        true -> (RepresentedValue - VNGMinValue) / (VNGMaxValue - VNGMinValue)
                                    end
                            end,
                            report:node_stimulated(WriteToLog, self(), Source, ResStimulus, Stimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),
                            stimulation:respond_to_stimulation(Source, ResStimulus),
                            process_events(State#state{last_excitation=CurrExcitation, last_stimulation_id=StimulationId})
                    end;

                CurrVNGMode ->
                    report:node_stimulated(WriteToLog, self(), Source, NewExcitation, Stimulus, ExperimentStep, StimulationName, CurrDepth, Reporter),
                    
                    % VN -> VN
                    NeighVNsToStimulate = case VNType of
                        categorical -> [];
                        numerical -> [Neigh || Neigh <- tuple_to_list(ConnectedVNs), Neigh =/= none, element(1, Neigh) =/= Source]
                    end,

                    StimulatedVNs = case CurrVNGMode of
                        accumulative -> [];
                        transitive -> lists:foldl(
                                fun({VN, NeighValue}, Acc) ->
                                    NeighStimulus = Stimulus * weight_vn_to_vn(NeighValue, RepresentedValue, VNGMinValue, VNGMaxValue),
                                    if 
                                        NeighStimulus >= MinPassedStimulus -> 
                                            vn:stimulate(VN, NeighStimulus, NewDepth, StimulationSpec),
                                            [VN | Acc];
                                        true -> Acc
                                    end
                                end,
                                [],
                                NeighVNsToStimulate
                            )
                        end,

                    % VN -> ON
                    ONStimulus = case StimulationKind of
                        poisoning -> Stimulus; % * (1.0 - weight_vn_to_on(ConnectedONs, EntireVNGConnCount));
                        _ -> Stimulus * weight_vn_to_on(ConnectedONs, EntireVNGConnCount)
                    end,

                    StimulatedONs = case CurrVNGMode of
                        accumulative -> [];
                        transitive ->
                            if
                                ONStimulus >= MinPassedStimulus -> 
                                    [ON || ON <- maps:keys(ConnectedONs), ON =/= Source];
                                true -> []
                            end
                    end,
                    
                    lists:foreach(
                        fun(ON) -> on:stimulate(ON, ONStimulus, NewDepth, StimulationSpec) end, 
                        StimulatedONs
                    ),


                    % Stimulated neighs
                    
                    NewStimulatedNeighs = case StimulatedONs ++ StimulatedVNs of
                        [] ->
                            stimulation:send_stimulation_finished(Source, CurrDepth),
                            StimulatedNeighs;
                        
                        StimulatedONsAndVNs ->
                            case StimulatedNeighs of
                                #{NewDepth := {StimulatedNeighsAtDepth, SourcesAtDepth}} -> 
                                    NewStimulatedNeighsAtDepth = lists:foldl( 
                                        fun(Neigh, Acc) ->
                                            case Acc of
                                                #{Neigh := StimulationCount} -> Acc#{Neigh => StimulationCount + 1};
                                                _ -> Acc#{Neigh => 1}
                                            end
                                        end, 
                                        StimulatedNeighsAtDepth, 
                                        StimulatedONsAndVNs
                                    ),
                                    NewSourcesAtDepth = case SourcesAtDepth of
                                        #{Source := SourceStimulationCount} -> SourcesAtDepth#{Source => SourceStimulationCount + 1};
                                        _ -> SourcesAtDepth#{Source => 1}
                                    end,
                                    StimulatedNeighs#{NewDepth => {NewStimulatedNeighsAtDepth, NewSourcesAtDepth}};
                                    
                                _ -> 
                                    StimulatedNeighs#{NewDepth => {
                                        lists:foldl( fun(Neigh, Acc) -> Acc#{Neigh => 1} end, #{}, StimulatedONsAndVNs),
                                        #{Source => 1}
                                    }}
                            end
                    end,

                    % Accumulate
                    process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId, stimulated_neighs=NewStimulatedNeighs})
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


        {connect_VN, Asker, NewConnectedVN, ConnectedVNValue, ExperimentStep} ->
            NewConnectedVNs = if
                % no need to handle categorical VN case - connect_VN should never be called on such VN
                ConnectedVNValue < RepresentedValue -> 
                    {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
                    report_breaking_connection(LeftConnectedVN, NewConnectedVN, ExperimentStep, GlobalCfg),
                    {{NewConnectedVN, ConnectedVNValue}, RightConnectedVN};

                ConnectedVNValue > RepresentedValue ->
                    {LeftConnectedVN, _RightConnectedVN} = ConnectedVNs,
                    %% DON'T report_breaking_connection(RightConnectedVN, NewConnectedVN, ExperimentStep, GlobalCfg) - your neighbour will do it
                    {LeftConnectedVN, {NewConnectedVN, ConnectedVNValue}}
            end,

            Asker ! {vn_connected, self(), NewConnectedVN},
            process_events(State#state{connected_vns=NewConnectedVNs});


        {connect_ON, Asker, ON, ONIndex} -> 
            Asker ! {on_connected, self(), ON},
            process_events(State#state{connected_ons=ConnectedONs#{ON => ONIndex}});


        {disconnect_ON, ON, ExperimentStep} ->
            NewConnectedONs = maps:remove(ON, ConnectedONs),
            vng:notify_VNG_to_ON_conn_count_decremented(VNG),
            
            if 
                map_size(NewConnectedONs) == 0 -> 
                    report:node_killed(self(), ExperimentStep, Reporter),
                    
                    VNsPendingDisconnection = case VNType of
                        categorical -> [];

                        numerical ->
                            {LeftConnectedVN, RightConnectedVN} = ConnectedVNs,
                            case LeftConnectedVN of
                                none -> ok;
                                {LeftVN, _LeftReprValue} -> vn:disconnect_VN(LeftVN, left, self(), RightConnectedVN, sets:new([{version, 2}]), ExperimentStep)
                            end,
                            case RightConnectedVN of
                                none -> ok;
                                {RightVN, _RightReprValue} -> vn:disconnect_VN(RightVN, right, self(), LeftConnectedVN, sets:new([{version, 2}]), ExperimentStep)
                            end,
                            [VN || {VN, _ReprValue} <- tuple_to_list(ConnectedVNs), VN /= none]
                    end,

                    vng:remove_killed_vn(VNG, RepresentedValue, self()),
                    zombie_wait_for_orhpan_messages(StimulatedNeighs, ConnectedVNs, sets:from_list(VNsPendingDisconnection, [{version, 2}]), ON);

                true -> 
                    on:confirm_death_notification(ON, self()),
                    process_events(State#state{connected_ons=NewConnectedONs})
            end;


        {disconnect_VN, Direction, DisconnectedVN, ReplacementVN, IntermediateZombies, ExperimentStep} ->
            NewConnectedVNs = case Direction of
                left ->
                    case ConnectedVNs of
                        {_LeftConnectedVN, none} -> 
                            ConnectedVNs;
                        {LeftConnectedVN, {RightConnectedVN, RightConnectedVNValue}} ->
                            if
                                DisconnectedVN =:= RightConnectedVN ->
                                    report_breaking_connection({DisconnectedVN, na}, ReplacementVN, ExperimentStep, GlobalCfg),
                                    {LeftConnectedVN, ReplacementVN};
                                ReplacementVN =:= none orelse element(2, ReplacementVN) > RightConnectedVNValue -> 
                                    report_breaking_connection({DisconnectedVN, na}, ReplacementVN, ExperimentStep, GlobalCfg),
                                    {LeftConnectedVN, ReplacementVN};
                                true -> ConnectedVNs
                            end
                    end;

                right ->
                    case ConnectedVNs of
                        {none, _RightConnectedVN} -> 
                            ConnectedVNs;
                        {{LeftConnectedVN, LeftConnectedVNValue}, RightConnectedVN} ->
                            if
                                DisconnectedVN =:= LeftConnectedVN ->
                                    report_breaking_connection({DisconnectedVN, na}, ReplacementVN, ExperimentStep, GlobalCfg),
                                    % I<3Ola; Nokia: connecting_people; I<3Ola
                                    {ReplacementVN, RightConnectedVN};
                                ReplacementVN =:= none orelse element(2, ReplacementVN) < LeftConnectedVNValue -> 
                                    report_breaking_connection({DisconnectedVN, na}, ReplacementVN, ExperimentStep, GlobalCfg),
                                    {ReplacementVN, RightConnectedVN};
                                true -> ConnectedVNs
                            end
                    end
            end,

            vn:confirm_vn_disconnected(DisconnectedVN, self(), IntermediateZombies),
            case ReplacementVN of
                none -> ok;
                {ReplVN, _ReplVNValue} -> report:connection_formed(self(), ReplVN, ExperimentStep, Reporter)
            end,
            process_events(State#state{connected_vns=NewConnectedVNs});


        {update_VNG_range, NewVNGMinValue, NewVNGMaxValue} -> 
            process_events(State#state{vng_min_value=NewVNGMinValue, vng_max_value=NewVNGMaxValue});


        {update_VNG_to_ON_conn_count, NewEntireVNGConnCount} ->
            process_events(State#state{vng_to_on_conn_count=NewEntireVNGConnCount});


        {get_excitation, Asker, LastAgdsStimulationId} -> 
            Excitation = case LastAgdsStimulationId of
                LastStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            Asker ! {last_excitation, self(), Excitation},
            process_events(State);
        

        {get_neighbours, Asker} -> 
            NeighbouringVNs = case VNType of
                categorical -> [];
                numerical -> 
                    NeighVNs = [Neigh || Neigh <- tuple_to_list(ConnectedVNs), Neigh /= none],
                    [{vn, VNGName, VNValue} || {_VN, VNValue} <- NeighVNs]
            end,
            NeighbouringONs = [{on, ONIndex} || ONIndex <- maps:values(ConnectedONs)],
            Asker ! {neighbours, self(), NeighbouringVNs ++ NeighbouringONs},
            process_events(State);


        {get_neigh_vns, Asker} ->
            Asker ! {neigh_vns, self(), ConnectedVNs},
            process_events(State);

    
        delete -> ok;


        reset_after_deadlock ->
            VNG ! {reset_after_deadlock_finished, self()},
            process_events(State#state{stimulated_neighs=#{}})
            
    end.



weight_vn_to_vn(TargetReprValue, OwnReprValue, VNGMinValue, VNGMaxValue) ->
    1.0 - abs(TargetReprValue - OwnReprValue) / (VNGMaxValue - VNGMinValue).


weight_vn_to_on(ConnectedONs, EntireVNGConnCount) ->
    VNConnCount = maps:size(ConnectedONs),

    if 
        EntireVNGConnCount == VNConnCount -> 0.0;
        true -> (EntireVNGConnCount - maps:size(ConnectedONs)) / EntireVNGConnCount
    end.


report_breaking_connection(none, _NewConnectedVN, _ExperimentStep, _GlobalCfg) -> ok;

report_breaking_connection({OldConnectedVN, _OldConnectedVNValue}, NewConnectedVN, _ExperimentStep, _GlobalCfg) when OldConnectedVN == NewConnectedVN -> ok;

report_breaking_connection({OtherVN, _OtherVNValue}, _NewConnectedVN, ExperimentStep, #global_cfg{reporter=Reporter}) -> report:connection_broken(self(), OtherVN, ExperimentStep, Reporter).


zombie_wait_for_orhpan_messages(StimulatedNeighs, LastConnectedVNs, VNsPendingDisconnection, ONPendingConfirmation) ->
    AreAnyVNsPendingDisconnection = sets:is_empty(VNsPendingDisconnection),

    NewONPendingConfirmation = if
        ONPendingConfirmation =:= none -> none;
        AreAnyVNsPendingDisconnection -> 
            on:confirm_death_notification(ONPendingConfirmation, self()),
            none;
        true -> ONPendingConfirmation
    end,

    receive
        {stimulate, Source, _Stimulus, Depth, _StimulationSpec} ->
            stimulation:send_stimulation_finished(Source, Depth),
            zombie_wait_for_orhpan_messages(StimulatedNeighs, LastConnectedVNs, VNsPendingDisconnection, NewONPendingConfirmation);

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
            zombie_wait_for_orhpan_messages(NewStimulatedNeighs, LastConnectedVNs, VNsPendingDisconnection, NewONPendingConfirmation);

        {disconnect_VN, Direction, DisconnectedVN, ReplacementVN, IntermediateZombies, ExperimentStep} ->
            {LeftConnectedVN, RightConnectedVN} = LastConnectedVNs,

            case Direction of
                left ->
                    case LeftConnectedVN of
                        none -> 
                            vn:confirm_vn_disconnected(DisconnectedVN, self(), IntermediateZombies);
                        {LeftConnectedVNPid, _LeftConnectedVNValue} -> 
                            vn:disconnect_VN(LeftConnectedVNPid, left, DisconnectedVN, ReplacementVN, sets:add_element(self(), IntermediateZombies), ExperimentStep)
                    end;
                right ->
                    case RightConnectedVN of
                        none ->
                            vn:confirm_vn_disconnected(DisconnectedVN, self(), IntermediateZombies);
                        {RightConnectedVNPid, _RightConnectedVNValue} -> 
                            vn:disconnect_VN(RightConnectedVNPid, right, DisconnectedVN, ReplacementVN, sets:add_element(self(), IntermediateZombies), ExperimentStep)
                    end
            end,
            zombie_wait_for_orhpan_messages(StimulatedNeighs, LastConnectedVNs, VNsPendingDisconnection, NewONPendingConfirmation);

        {confirm_vn_disconnected, VN, IntermediateZombies} ->
            IsConfirmationFromNeigh = sets:is_element(VN, VNsPendingDisconnection),
            IsConfirmationFromNodeAfar = not sets:is_empty(sets:intersection(IntermediateZombies, VNsPendingDisconnection)),

            NewVNsPendingDisconnection = if
                IsConfirmationFromNeigh ->
                    sets:del_element(VN, VNsPendingDisconnection);
                IsConfirmationFromNodeAfar ->
                    [MatchingZombie] = sets:to_list(sets:intersection(IntermediateZombies, VNsPendingDisconnection)),
                    sets:del_element(MatchingZombie, VNsPendingDisconnection);
                true -> 
                    VNsPendingDisconnection
            end,
            zombie_wait_for_orhpan_messages(StimulatedNeighs, LastConnectedVNs, NewVNsPendingDisconnection, NewONPendingConfirmation)

    after 5000 -> killed
    end.
