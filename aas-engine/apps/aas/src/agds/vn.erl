-module(vn).
-export([create_VN/8, 
        connect_VN/3, 
        connect_ON/3, 
        disconnect_ON/2,
        disconnect_VN/3,
        update_VNG_range/3, 
        update_VNG_to_ON_conn_count/2,
        stimulate/4, 
        get_excitation/2,
        get_neighbours/1,
        delete/1
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

create_VN(categorical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, GlobalCfg) -> 
    spawn(fun() -> init(categorical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, GlobalCfg) end);

create_VN(numerical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, GlobalCfg) -> 
    spawn(fun() -> init(numerical, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, GlobalCfg) end).


connect_VN(ThisVN, ConnectedVN, ConnectedVNValue) -> ThisVN ! {connect_VN, ConnectedVN, ConnectedVNValue}.


connect_ON(ThisVN, ON, ONIndex) -> ThisVN ! {connect_ON, ON, ONIndex}.


disconnect_ON(ThisVN, ON) -> ThisVN ! {disconnect_ON, ON}.


disconnect_VN(ThisVN, DisconnectedVN, ReplacementVN) -> ThisVN ! {disconnect_VN, DisconnectedVN, ReplacementVN}.


update_VNG_range(ThisVN, NewVNGMinValue, NewVNGMaxValue) -> ThisVN ! {update_VNG_range, NewVNGMinValue, NewVNGMaxValue}.


update_VNG_to_ON_conn_count(ThisVN, NewEntireVNGConnCount) -> ThisVN ! {update_VNG_to_ON_conn_count, NewEntireVNGConnCount}.


stimulate(ThisVN, Stimulus, CurrDepth, StimulationSpec) -> 
    ThisVN ! {stimulate, self(), Stimulus, CurrDepth, StimulationSpec}.


get_excitation(ThisVN, LastStimulationId) -> 
    ThisVN ! {get_excitation, self(), LastStimulationId},
    % io:format("~s    VN ~p (VNG: ~p): get_excitation sent~n", [utils:get_timestamp_str(), ThisVN, self()]),
    receive
        {last_excitation, Excitation} -> Excitation;
        {remove_killed_vn, ReprValue, ThisVN} -> 
            vng:remove_killed_vn(self(), ReprValue, ThisVN),
            none
    end.


get_neighbours(ThisVN) -> ThisVN ! {get_neighbours, self()},
    receive
        {neighbours, Neighbours} -> Neighbours;
        {remove_killed_vn, ReprValue, ThisVN} -> 
            vng:remove_killed_vn(self(), ReprValue, ThisVN),
            []
    end.


delete(ThisVN) -> ThisVN ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(VNType, RepresentedValue, VNG, VNGName, EntireVNGConnCount, VNGMinValue, VNGMaxValue, #global_cfg{reporter=Reporter} = GlobalCfg) ->
    report:node_creation(self(), vn, RepresentedValue, VNG, Reporter),
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
                                stimulation:respond_to_stimulation(Source, ResStimulus),
                                process_events(State#state{last_excitation=CurrExcitation, last_stimulation_id=StimulationId})
                    end;

                CurrVNGMode ->
                    
                    % VN -> VN
                    NeighVNsToStimulate = case VNType of
                        categorical -> [];
                        numerical -> [Neigh || Neigh <- tuple_to_list(ConnectedVNs), Neigh =/= none, element(1, Neigh) =/= Source]
                    end,

                    StimulatedVNs = lists:foldl(
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
                    ),

                    % VN -> ON
                    ONStimulus = Stimulus * weight_vn_to_on(ConnectedONs, EntireVNGConnCount),

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

                    % io:format(
                    %     "~s    VN ~p: stimulated nodes ~p, new stimulated neighs: ~p~n", 
                    %     [utils:get_timestamp_str(), self(), StimulatedONs ++ StimulatedVNs, NewStimulatedNeighs]
                    % ),

                    % Accumulate
                    process_events(State#state{last_excitation=NewExcitation, last_stimulation_id=StimulationId, stimulated_neighs=NewStimulatedNeighs})
            end;


        {stimulation_finished, StimulatedNode, Depth, ConfirmationCount} ->
            % io:format(
            %     "~s    VN ~p: stimulation_finished received from ~p at depth: ~p (confirmation count: ~p)~nstimulated neighs: ~p~n", 
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


        {connect_VN, NewConnectedVN, ConnectedVNValue} ->
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


        {connect_ON, ON, ONIndex} -> 
            process_events(State#state{connected_ons=ConnectedONs#{ON => ONIndex}});


        {disconnect_ON, ON} ->
            NewConnectedONs = maps:remove(ON, ConnectedONs),
            vng:notify_VNG_to_ON_conn_count_decremented(VNG),
            
            case NewConnectedONs of
                [] -> 
                    report:node_killed(self(), Reporter),

                    case VNType of
                        categorical -> ok;
                        numerical ->
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


        {update_VNG_range, NewVNGMinValue, NewVNGMaxValue} -> 
            process_events(State#state{vng_min_value=NewVNGMinValue, vng_max_value=NewVNGMaxValue});


        {update_VNG_to_ON_conn_count, NewEntireVNGConnCount} ->
            process_events(State#state{vng_to_on_conn_count=NewEntireVNGConnCount});


        {get_excitation, Asker, LastAgdsStimulationId} -> 
            % io:format("~s    VN ~p (VNG: ~p): get_excitation received~n", [utils:get_timestamp_str(), VNG, self()]),
            Excitation = case LastAgdsStimulationId of
                LastStimulationId -> LastExcitation;
                _ -> 0.0
            end,
            % io:format("~s    VN ~p (VNG: ~p): sending {last_excitation, ~p} back~n", [utils:get_timestamp_str(), VNG, self(), Excitation]),
            Asker ! {last_excitation, Excitation},
            process_events(State);
        

        {get_neighbours, Asker} -> 
            NeighbouringVNs = case VNType of
                categorical -> [];
                numerical -> 
                    NeighVNs = [Neigh || Neigh <- tuple_to_list(ConnectedVNs), Neigh /= none],
                    [{vn, VNGName, VNValue} || {_VN, VNValue} <- NeighVNs]
            end,
            NeighbouringONs = [{on, ONIndex} || ONIndex <- maps:values(ConnectedONs)],
            Asker ! {neighbours, NeighbouringVNs ++ NeighbouringONs},
            process_events(State);

    
        delete -> ok

    end.



weight_vn_to_vn(TargetReprValue, OwnReprValue, VNGMinValue, VNGMaxValue) ->
    1.0 - abs(TargetReprValue - OwnReprValue) / (VNGMaxValue - VNGMinValue).


weight_vn_to_on(ConnectedONs, EntireVNGConnCount) ->
    %% CLASSICAL WEIGHT
    % case length(ConnectedONs) > 0 of
    %     true ->  1 / length(ConnectedONs);
    %     false -> 0.0
    % end.
    
    %% MODIFIED WEIGHT
    
    VNConnCount = maps:size(ConnectedONs),

    if 
        EntireVNGConnCount == VNConnCount -> 1.0;
        true -> (EntireVNGConnCount - maps:size(ConnectedONs)) / EntireVNGConnCount
    end.


report_breaking_connection(none, _NewConnectedVN, _GlobalCfg) -> ok;

report_breaking_connection({OldConnectedVN, _OldConnectedVNValue}, NewConnectedVN, _GlobalCfg) when OldConnectedVN == NewConnectedVN -> ok;

report_breaking_connection({OtherVN, _OtherVNValue}, _NewConnectedVN, #global_cfg{reporter=Reporter}) -> report:connection_broken(self(), OtherVN, Reporter).


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
