-module(vng).
-export([create_numerical_VNG/4, create_categorical_VNG/3, add_value/5, wait_for_value_added/1, stimulate/3, get_excitation/2, get_neighbours/2, get_number_of_nodes/1, delete/1]).
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

create_numerical_VNG(VNGName, Epsilon, AGDS, GlobalCfg) -> spawn(fun() -> init(numerical, VNGName, Epsilon, AGDS, GlobalCfg) end).

create_categorical_VNG(VNGName, AGDS, GlobalCfg) -> spawn(fun() -> init(categorical, VNGName, no_epsilon, AGDS, GlobalCfg) end).


add_value(ExperimentStep, VNG, AddedValue, RespectiveON, RespectiveONIndex) -> VNG ! {add_value, AddedValue, RespectiveON, RespectiveONIndex, ExperimentStep}.

wait_for_value_added(VNG) -> receive {value_added, VNG} -> ok end.


stimulate(VNG, Stimuli, StimulationSpec) -> 
    VNG ! {stimulate, Stimuli, StimulationSpec}.



get_excitation(VNG, LastStimulationId) -> 
    VNG ! {get_excitation, self(), LastStimulationId}, 
    receive 
        {vns_excitation, VNsExcitation} -> VNsExcitation
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
    });

init(numerical, VNGName, Epsilon, AGDS, GlobalCfg) ->
    report_vng_creation(numerical, VNGName, GlobalCfg),
    process_events(#state{
        vng_type=numerical, 
        vng_name=VNGName, 
        vns=avb_tree:create(Epsilon), 
        min_value=none, 
        max_value=none, 
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
        {add_value, AddedValue, RespectiveON, RespectiveONIndex, ExperimentStep} ->
            if
                MinValue =:= none, MaxValue =:= none -> NewMinValue = AddedValue, NewMaxValue = AddedValue;
                AddedValue < MinValue -> NewMinValue = AddedValue, NewMaxValue = MaxValue, update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue);
                AddedValue > MaxValue -> NewMinValue = MinValue, NewMaxValue = AddedValue, update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue);
                true -> NewMinValue = MinValue, NewMaxValue = MaxValue
            end,

            case VNGType of
                categorical -> 
                    case maps:find(AddedValue, VNs) of
                        {ok, VN} -> NewVNs = VNs;
                        error -> 
                            VN = vn:create_VN(categorical, AddedValue, self(), VNGName, VNGtoONConnCount, NewMinValue, NewMaxValue, ExperimentStep, GlobalCfg),
                            NewVNs = VNs#{AddedValue => VN}
                    end;
                
                numerical -> 
                    {NewVNs, {IsNew, VN}} = avb_tree:add(VNs, AddedValue, fun() -> vn:create_VN(numerical, AddedValue, self(), VNGName, VNGtoONConnCount, NewMinValue, NewMaxValue, ExperimentStep, GlobalCfg) end),
                        
                    case IsNew of
                        new_value ->
                            Neighs = avb_tree:get_neighbours(NewVNs, AddedValue),

                            lists:foreach(
                                fun(Neigh) -> case Neigh of
                                    none -> ok;
                                    {NeighReprValue, NeighVN} ->
                                        vn:connect_VN(VN, NeighVN, NeighReprValue, ExperimentStep),
                                        vn:connect_VN(NeighVN, VN, AddedValue, ExperimentStep),
                                        report:connection_formed(VN, NeighVN, ExperimentStep, Reporter)
                                    end
                                end, 
                                tuple_to_list(Neighs)
                            );

                        existing_value -> ok
                    end
            end,

            vn:connect_ON(VN, RespectiveON, RespectiveONIndex),
            on:connect_VN(RespectiveON, VN, AddedValue, VNGName),
            report:connection_formed(VN, RespectiveON, ExperimentStep, Reporter),

            NewAllVNsSet = sets:add_element(VN, AllVNsSet),

            NewVNGtoONConnCount = VNGtoONConnCount + 1,
            update_VNG_to_ON_conn_count(NewAllVNsSet, NewVNGtoONConnCount),

            AGDS ! {value_added, self()},

            process_events(State#state{vns=NewVNs, min_value=NewMinValue, max_value=NewMaxValue, all_vns_set=NewAllVNsSet, vng_to_on_conn_count=NewVNGtoONConnCount});


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
                                case avb_tree:get_nearest(VNs, Value) of
                                    {exact_match, VN} -> 
                                        vn:stimulate(VN, Stimulus, 0, StimulationSpec),
                                        sets:add_element(VN, Acc);
                                    {none, none} -> 
                                        Acc;
                                    {LeftNeigh, RightNeigh} ->
                                        NewAccL = case LeftNeigh of
                                            none -> sets:new();
                                            {LeftVNValue, LeftVN} -> 
                                                vn:stimulate(LeftVN, get_nearby_VN_stimulus(Value, LeftVNValue, MinValue, MaxValue, Stimulus), 0, StimulationSpec),
                                                sets:from_list([LeftVN])
                                        end,
                                        NewAccR = case RightNeigh of
                                            none -> sets:new();
                                            {RightVNValue, RightVN} -> 
                                                vn:stimulate(RightVN, get_nearby_VN_stimulus(Value, RightVNValue, MinValue, MaxValue, Stimulus), 0, StimulationSpec),
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


        {get_excitation, Caller, LastStimulationId} ->
            VNsResponses = case VNGType of
                categorical -> maps:map(fun(_ReprValue, VN) -> vn:get_excitation(VN, LastStimulationId) end, VNs);
                numerical -> #{ReprValue => vn:get_excitation(VN, LastStimulationId) || {ReprValue, VN, _Occurances} <- avb_tree:items(VNs)}    %% TODO
            end,

            VNsExcitation = maps:filter(fun(_ReprValue, Exc) -> Exc /= none end, VNsResponses),
            Caller ! {vns_excitation, VNsExcitation},
            process_events(State);


        {get_neighbours, Value, Asker} ->
            VN = case VNGType of
                categorical ->
                    case maps:find(Value, VNs) of
                        {ok, CatSourceVN} -> CatSourceVN;
                        error -> none
                    end;

                numerical ->
                    case avb_tree:get(VNs, Value) of
                        {NumSourceVN, _Occurances} -> NumSourceVN;
                        none -> none
                    end
                end,

            Neighbours = case VN of
                none -> [];
                _ -> vn:get_neighbours(VN)
            end,

            Asker ! {neighbours, Neighbours},
            process_events(State);


        {get_number_of_nodes, Asker} ->
            Asker ! {number_of_nodes, sets:size(AllVNsSet)},
            process_events(State);


        {remove_killed_vn, RemovedValue, RemovedVN} ->
            NewVNs = case VNGType of
                categorical ->
                    maps:remove(RemovedValue, VNs);

                numerical ->
                    RemainingVNs = lists:flatten([lists:duplicate(Occurances, {VN, ReprValue}) || {ReprValue, VN, Occurances} <- avb_tree:items(VNs), abs(ReprValue - RemovedValue) >= VNs#tree.epsilon]),
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


vng_range(none, none) -> 1.0;

vng_range(TheOnlyValue, TheOnlyValue) -> 1.0;

vng_range(MinValue, MaxValue) -> MaxValue - MinValue.


update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue) ->
    lists:foreach(fun(VN) -> vn:update_VNG_range(VN, NewMinValue, NewMaxValue) end, sets:to_list(AllVNsSet)).


update_VNG_to_ON_conn_count(AllVNsSet, NewVNGtoONConnCount) ->
    lists:foreach(fun(VN) -> vn:update_VNG_to_ON_conn_count(VN, NewVNGtoONConnCount) end, sets:to_list(AllVNsSet)).


get_nearby_VN_stimulus(ExactValue, _VNReprValue, MinVNGValue, MaxVNGValue, Stimulus) when ExactValue < MinVNGValue; ExactValue > MaxVNGValue -> 
    Stimulus;

get_nearby_VN_stimulus(ExactValue, VNReprValue, MinVNGValue, MaxVNGValue, Stimulus) -> 
    Stimulus * (1 - abs(ExactValue - VNReprValue) / vng_range(MinVNGValue, MaxVNGValue)).
