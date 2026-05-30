-module(vng).
-export([create_numerical_VNG/6, create_categorical_VNG/3, add_value/5, wait_for_value_added/1, reconnect_vn_to_on/5,
        ensure_vn/3, wait_for_vn_ensured/1, connect_vn_to_on/5, wait_for_vn_connected_to_on/1,
        stimulate/3, get_excitation/2, get_vn/2, get_all_vns/1, get_neighbours/2, get_number_of_nodes/1, delete/1]).
-export([remove_killed_vn/3, notify_VNG_to_ON_conn_count_incremented/1, notify_VNG_to_ON_conn_count_decremented/1]).
-export([reset_after_deadlock/1]).
-export([print_neighbourhoods/1]).

-include("config.hrl").
-include("stimulation.hrl").
-include("../generic/avb_tree.hrl").

-record(state, {
    vng_type,               % categorical | numerical
    vng_name,               % string
    vns,                    % #{repr_value := pid} | avb_tree:tree()
    min_value,              % float
    max_value,              % float
    all_vns_set,            % sets:set()
    vng_to_on_conn_count,   % integer
    stimulated_vns,         % sets:set()
    agds,                   % pid
    global_cfg              % #global_cfg{}
}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_numerical_VNG(VNGName, Epsilon, MinValue, MaxValue, AGDS, GlobalCfg) -> spawn(fun() -> init(numerical, VNGName, Epsilon, MinValue, MaxValue, AGDS, GlobalCfg) end).

create_categorical_VNG(VNGName, AGDS, GlobalCfg) -> spawn(fun() -> init(categorical, VNGName, no_epsilon, AGDS, GlobalCfg) end).

add_value(ExperimentStep, VNG, AddedValue, RespectiveON, RespectiveONIndex) -> VNG ! {add_value, AddedValue, RespectiveON, RespectiveONIndex, ExperimentStep}.

wait_for_value_added(VNG) -> receive {value_added, VNG, VN} -> {ok, VN} end.


ensure_vn(VNG, Value, ExperimentStep) -> VNG ! {ensure_vn, Value, ExperimentStep}.

wait_for_vn_ensured(VNG) -> receive {vn_ensured, VNG, VN, IsNew} -> {VN, IsNew} end.


connect_vn_to_on(VNG, Value, ON, ONIndex, ExperimentStep) -> VNG ! {connect_vn_to_on, Value, ON, ONIndex, ExperimentStep}.

wait_for_vn_connected_to_on(VNG) -> receive {vn_connected_to_on, VNG} -> ok end.


reconnect_vn_to_on(VNG, VNValue, NewON, NewONIndex, ExperimentStep) ->
    VNG ! {reconnect_vn_to_on, VNValue, NewON, NewONIndex, ExperimentStep, self()},
    receive
        {vn_reconnected_to_on, _VN} -> ok
    end.

 
stimulate(VNG, Stimuli, StimulationSpec) -> 
    VNG ! {stimulate, Stimuli, StimulationSpec}.



get_excitation(VNG, LastStimulationId) -> 
    VNG ! {get_excitation, LastStimulationId, self()}, 
    receive 
        {vns_excitation, VNsExcitation} -> VNsExcitation
    end.


get_vn(VNG, ReprValue) ->
    VNG ! {get_vn, ReprValue, self()},
    receive
        {vn, none} -> none;
        {vn, VN} -> VN
    end.


get_all_vns(VNG) ->
    VNG ! {get_all_vns, self()},
    receive
        {all_vns, VNs} -> VNs
    end.


get_neighbours(VNG, Value) -> 
    VNG ! {get_neighbours, Value, self()},
    receive
        {neighbours, Neighbours} -> Neighbours
    end.


get_number_of_nodes(VNG) ->
    VNG ! {get_number_of_nodes, self()},
    receive
        {number_of_nodes, NumberOfNodes} -> NumberOfNodes
    end.


remove_killed_vn(VNG, RemovedValue, RemovedVN) -> VNG ! {remove_killed_vn, RemovedValue, RemovedVN}.


notify_VNG_to_ON_conn_count_incremented(VNG) -> VNG ! {notify_VNG_to_ON_conn_count_incremented}.

notify_VNG_to_ON_conn_count_decremented(VNG) -> VNG ! {notify_VNG_to_ON_conn_count_decremented}.


delete(VNG) -> VNG ! delete.


reset_after_deadlock(VNG) -> 
    VNG ! reset_after_deadlock,
    receive
        {reset_after_deadlock_finished, VNG} -> ok
    end.


print_neighbourhoods(VNG) ->
    VNG ! print_neighbourhoods.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(categorical, VNGName, no_epsilon, AGDS, GlobalCfg) ->
    report_vng_creation(categorical, VNGName, GlobalCfg),
    process_events(#state{
        vng_type=categorical, 
        vng_name=VNGName, 
        vns=#{}, 
        min_value=none, 
        max_value=none, 
        all_vns_set=sets:new(), 
        vng_to_on_conn_count=0, 
        stimulated_vns=sets:new(),
        agds=AGDS, 
        global_cfg=GlobalCfg
    }).

init(numerical, VNGName, Epsilon, MinValue, MaxValue, AGDS, GlobalCfg) ->
    report_vng_creation(numerical, VNGName, GlobalCfg),
    process_events(#state{
        vng_type=numerical, 
        vng_name=VNGName, 
        vns=avb_tree:create(Epsilon), 
        min_value=MinValue, 
        max_value=MaxValue, 
        all_vns_set=sets:new(), 
        vng_to_on_conn_count=0, 
        stimulated_vns=sets:new(),
        agds=AGDS, 
        global_cfg=GlobalCfg
    }).


report_vng_creation(VNGType, VNGName, #global_cfg{reporter=Reporter}) ->
    report:node_group_creation(self(), VNGName, {vng, VNGType}, Reporter).


% Separate AllVNsSet is stored to quicken sending messages to all VNs within VNG (could be replaced with pg:)
process_events(#state{
    vng_type=VNGType, 
    vng_name=VNGName, 
    vns=VNs, 
    min_value=MinValue, 
    max_value=MaxValue, 
    all_vns_set=AllVNsSet, 
    vng_to_on_conn_count=VNGtoONConnCount, 
    stimulated_vns=StimulatedVNs,
    agds=AGDS, 
    global_cfg=#global_cfg{reporter=Reporter} = GlobalCfg
} = State) ->

    receive

        {ensure_vn, AddedValue, ExperimentStep} ->
            AddedValueBounded = bounded_value(AddedValue, VNGType, MinValue, MaxValue),

            case VNGType of
                categorical -> 
                    case maps:find(AddedValueBounded, VNs) of
                        {ok, VN} -> 
                            NewVNs = VNs,
                            IsNew = existing;
                        error -> 
                            VN = vn:create_VN(categorical, AddedValueBounded, self(), VNGName, VNGtoONConnCount, MinValue, MaxValue, ExperimentStep, GlobalCfg),
                            NewVNs = VNs#{AddedValueBounded => VN},
                            IsNew = new
                    end;
                
                numerical -> 
                    {NewVNs, {IsNewRaw, VN}} = avb_tree:add(VNs, AddedValueBounded, fun() -> vn:create_VN(numerical, AddedValueBounded, self(), VNGName, VNGtoONConnCount, MinValue, MaxValue, ExperimentStep, GlobalCfg) end),
                        
                    IsNew = case IsNewRaw of
                        new_value -> new;
                        existing_value -> existing
                    end,

                    case IsNewRaw of
                        new_value ->
                            Neighs = avb_tree:get_neighbours(NewVNs, AddedValueBounded),

                            lists:foreach(
                                fun(Neigh) -> case Neigh of
                                    none -> ok;
                                    {NeighReprValue, NeighVN} ->
                                        vn:connect_VN(VN, NeighVN, NeighReprValue, ExperimentStep),
                                        vn:connect_VN(NeighVN, VN, AddedValueBounded, ExperimentStep),
                                        report:connection_formed(VN, NeighVN, ExperimentStep, Reporter)
                                    end
                                end, 
                                tuple_to_list(Neighs)
                            );

                        existing_value -> ok
                    end
            end,

            NewAllVNsSet = sets:add_element(VN, AllVNsSet),
            AGDS ! {vn_ensured, self(), VN, IsNew},
            process_events(State#state{vns=NewVNs, all_vns_set=NewAllVNsSet});


        {connect_vn_to_on, Value, ON, ONIndex, ExperimentStep} ->
            VN = get_vn_for_value(Value, State),

            case vn:connect_ON(VN, ON, ONIndex) of
                already_connected -> 
                    NewVNGtoONConnCount = VNGtoONConnCount;
                ok -> 
                    on:connect_VN(ON, VN, VNGName),
                    report:connection_formed(VN, ON, ExperimentStep, Reporter),
                    NewVNGtoONConnCount = VNGtoONConnCount + 1
            end,

            update_VNG_to_ON_conn_count(AllVNsSet, NewVNGtoONConnCount),
            AGDS ! {vn_connected_to_on, self()},
            process_events(State#state{vng_to_on_conn_count=NewVNGtoONConnCount});


        {stimulate, Stimuli, #stim_spec{node_group_modes=NodeGroupModes}=StimulationSpec} ->
            case maps:get(VNGName, NodeGroupModes) of
                passive -> 
                    stimulation:send_stimulation_finished(AGDS, 0),
                    process_events(State);

                _ -> 
                    case VNGType of
                        categorical ->
                            NewStimulatedVNs = maps:fold(fun(Value, Stimulus, Acc) ->
                                case maps:find(Value, VNs) of
                                    {ok, VN} -> 
                                        vn:stimulate(VN, Stimulus, 0, StimulationSpec),
                                        sets:add_element(VN, Acc);
                                    error -> 
                                        Acc
                                end
                            end, sets:new(), Stimuli);
        
                        numerical ->
                            NewStimulatedVNs = maps:fold(fun (Value, Stimulus, Acc) ->
                                BoundedValue = bounded_value(Value, VNGType, MinValue, MaxValue),
                                case avb_tree:get_nearest(VNs, BoundedValue) of
                                    {exact_match, VN} -> 
                                        vn:stimulate(VN, Stimulus, 0, StimulationSpec),
                                        sets:add_element(VN, Acc);
                                    {none, none} -> 
                                        Acc;
                                    {LeftNeigh, RightNeigh} ->
                                        NewAccL = case LeftNeigh of
                                            none -> sets:new();
                                            {LeftVNValue, LeftVN} -> 
                                                vn:stimulate(LeftVN, get_nearby_VN_stimulus(BoundedValue, LeftVNValue, MinValue, MaxValue, Stimulus), 0, StimulationSpec),
                                                sets:from_list([LeftVN])
                                        end,
                                        NewAccR = case RightNeigh of
                                            none -> sets:new();
                                            {RightVNValue, RightVN} -> 
                                                vn:stimulate(RightVN, get_nearby_VN_stimulus(BoundedValue, RightVNValue, MinValue, MaxValue, Stimulus), 0, StimulationSpec),
                                                sets:from_list([RightVN])
                                        end,
                                        sets:union(NewAccL, sets:union(NewAccR, Acc))
                                end
                            end, sets:new(), Stimuli)
                    end,
                    
                    case sets:is_empty(NewStimulatedVNs) of
                        true -> stimulation:send_stimulation_finished(AGDS, 0);
                        false -> ok
                    end,
        
                    process_events(State#state{stimulated_vns=NewStimulatedVNs})
            end;


        {stimulation_finished, StimulatedVN, 0, 1} ->
            NewStimulatedVNs = sets:del_element(StimulatedVN, StimulatedVNs),
            case sets:is_empty(NewStimulatedVNs) of
                true -> 
                    stimulation:send_stimulation_finished(AGDS, 0);
                false -> ok
            end,
            process_events(State#state{stimulated_vns=NewStimulatedVNs});


        {reconnect_vn_to_on, VNValue, NewON, NewONIndex, ExperimentStep, Caller} ->
            BoundedValue = bounded_value(VNValue, VNGType, MinValue, MaxValue),
            VN = case VNGType of
                categorical -> maps:get(BoundedValue, VNs);
                numerical -> 
                    {Pid, _Occurances} = avb_tree:get(VNs, BoundedValue),
                    Pid
            end,
            case vn:connect_ON(VN, NewON, NewONIndex) of
                already_connected -> ok;
                ok -> report:connection_formed(VN, NewON, ExperimentStep, Reporter)
            end,
            Caller ! {vn_reconnected_to_on, VN},
            process_events(State);


        {get_vn, Value, Caller} ->
            BoundedValue = bounded_value(Value, VNGType, MinValue, MaxValue),
            VN = case VNGType of
                categorical -> maps:get(BoundedValue, VNs, none);
                numerical -> 
                    case avb_tree:get(VNs, BoundedValue) of
                        {Pid, _Occurances} -> Pid;
                        none -> none
                    end
            end,
            Caller ! {vn, VN},
            process_events(State);


        {get_all_vns, Caller} ->
            VNsList = case VNGType of
                categorical -> maps:to_list(VNs);
                numerical -> avb_tree:items(VNs)
            end,
            Caller ! {all_vns, VNsList},
            process_events(State);


        {get_excitation, LastStimulationId, Caller} ->
            VNsResponses = case VNGType of
                categorical -> maps:map(fun(_ReprValue, VN) -> vn:get_excitation(VN, LastStimulationId) end, VNs);
                numerical -> maps:from_list([{ReprValue, vn:get_excitation(VN, LastStimulationId)} || {ReprValue, VN, _Occurances} <- avb_tree:items(VNs)])
            end,

            VNsExcitation = maps:filter(fun(_ReprValue, Exc) -> Exc /= none end, VNsResponses),
            Caller ! {vns_excitation, VNsExcitation},
            process_events(State);


        {get_neighbours, Value, Caller} ->
            BoundedValue = bounded_value(Value, VNGType, MinValue, MaxValue),
            VN = case VNGType of
                categorical ->
                    case maps:find(BoundedValue, VNs) of
                        {ok, CatSourceVN} -> CatSourceVN;
                        error -> none
                    end;

                numerical ->
                    case avb_tree:get(VNs, BoundedValue) of
                        {NumSourceVN, _Occurances} -> NumSourceVN;
                        none -> none
                    end
                end,

            Neighbours = case VN of
                none -> [];
                _ -> vn:get_neighbours(VN)
            end,

            Caller ! {neighbours, Neighbours},
            process_events(State);


        {get_number_of_nodes, Caller} ->
            Caller ! {number_of_nodes, sets:size(AllVNsSet)},
            process_events(State);


        {remove_killed_vn, RemovedValue, RemovedVN} ->
            BoundedValue = bounded_value(RemovedValue, VNGType, MinValue, MaxValue),
            NewVNs = case VNGType of
                categorical ->
                    maps:remove(BoundedValue, VNs);

                numerical ->
                    RemainingVNs = lists:flatten([lists:duplicate(Occurances, {VN, ReprValue}) || {ReprValue, VN, Occurances} <- avb_tree:items(VNs), abs(ReprValue - BoundedValue) >= VNs#tree.epsilon]),
                    lists:foldl(fun({VN, ReprValue}, AVBTree) -> {NewTree, {_, VN}} = avb_tree:add(AVBTree, ReprValue, fun () -> VN end), NewTree end, avb_tree:create(VNs#tree.epsilon), RemainingVNs)
            end,

            NewAllVNsSet = sets:del_element(RemovedVN, AllVNsSet),
            process_events(State#state{vns=NewVNs, all_vns_set=NewAllVNsSet});


        {notify_VNG_to_ON_conn_count_incremented} ->
            NewVNGtoONConnCount = VNGtoONConnCount + 1,
            update_VNG_to_ON_conn_count(AllVNsSet, NewVNGtoONConnCount),
            process_events(State#state{vng_to_on_conn_count=NewVNGtoONConnCount});


        {notify_VNG_to_ON_conn_count_decremented} ->
            NewVNGtoONConnCount = VNGtoONConnCount - 1,
            update_VNG_to_ON_conn_count(AllVNsSet, NewVNGtoONConnCount),
            process_events(State#state{vng_to_on_conn_count=VNGtoONConnCount - 1});


        delete ->
            case VNGType of
                categorical -> maps:foreach(fun(_Value, VN) -> vn:delete(VN) end, VNs);
                numerical -> avb_tree:foreach(fun(_Value, VN) -> vn:delete(VN) end, VNs)
            end;


        reset_after_deadlock ->
            case VNGType of
                categorical -> maps:foreach(fun(_Value, VN) -> vn:reset_after_deadlock(VN) end, VNs);
                numerical -> avb_tree:foreach(fun(_Value, VN) -> vn:reset_after_deadlock(VN) end, VNs)
            end,
            AGDS ! {reset_after_deadlock_finished, self()},
            process_events(State#state{stimulated_vns=sets:new()});


        print_neighbourhoods ->
            Neighbourhoods = [{VN, vn:get_neigh_vns(VN)} || VN <- sets:to_list(AllVNsSet)],
            FormattedNeighbourhoods = lists:foldl(
                fun ({VN, {LeftNeigh, RightNeigh}}, Acc) -> 
                    Acc ++ io_lib:format("~n~p <- ~p -> ~p", [LeftNeigh, VN, RightNeigh]) end, 
                "", 
                Neighbourhoods
            ),
            io:format("VNG<~p> - VN neighbourhoods: ~s~n", [VNGName, FormattedNeighbourhoods]),
            process_events(State)
    end.

vng_range(MinValue, MaxValue) -> MaxValue - MinValue.


update_VNG_to_ON_conn_count(AllVNsSet, NewVNGtoONConnCount) ->
    lists:foreach(fun(VN) -> vn:update_VNG_to_ON_conn_count(VN, NewVNGtoONConnCount) end, sets:to_list(AllVNsSet)).


get_nearby_VN_stimulus(ExactValue, _VNReprValue, MinVNGValue, MaxVNGValue, Stimulus) when ExactValue < MinVNGValue; ExactValue > MaxVNGValue -> 
    Stimulus;

get_nearby_VN_stimulus(ExactValue, VNReprValue, MinVNGValue, MaxVNGValue, Stimulus) -> 
    Stimulus - (1 - abs(ExactValue - VNReprValue) / vng_range(MinVNGValue, MaxVNGValue)).


get_vn_for_value(Value, #state{vng_type = VNGType, min_value = MinValue, max_value = MaxValue, vns = VNs}) ->
    ValueBounded = bounded_value(Value, VNGType, MinValue, MaxValue),
    case VNGType of
        categorical -> maps:get(ValueBounded, VNs);
        numerical -> 
            case avb_tree:get(VNs, ValueBounded) of
                {Pid, _Occurances} -> Pid;
                none -> none
            end
    end.


bounded_value(Value, categorical, _MinValue, _MaxValue) -> Value;
bounded_value(Value, _VNGType, MinValue, _MaxValue) when Value < MinValue -> MinValue;
bounded_value(Value, _VNGType, _MinValue, MaxValue) when Value > MaxValue -> MaxValue;
bounded_value(Value, _VNGType, _MinValue, _MaxValue) -> Value.