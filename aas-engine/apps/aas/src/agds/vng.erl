-module(vng).
-export([create_numerical_VNG/3, create_categorical_VNG/2, add_value/3, stimulate/5, reset_excitation/1, get_excitation/1, get_neighbours/2, delete/1]).

-include("config.hrl").

-record(state, {vng_type, vng_name, vns, min_value, max_value, all_vns_set, global_cfg}).


%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create_numerical_VNG(VNGName, Epsilon, GlobalCfg) -> spawn(fun() -> init(numerical, VNGName, Epsilon, GlobalCfg) end).

create_categorical_VNG(VNGName, GlobalCfg) -> spawn(fun() -> init(categorical, VNGName, no_epsilon, GlobalCfg) end).


add_value(VNG, AddedValue, RespectiveON) -> VNG ! {add_value, AddedValue, RespectiveON}.



stimulate(VNG, all, repr_value, MaxDepth, StimulationKind) -> VNG ! {stimulate, all, repr_value, MaxDepth, StimulationKind};

% Value - value observed on receptor, Stimulation - strength of stimulation, StimulationKind - infere | {poison, DeadlyDose}
stimulate(VNG, Value, Stimulation, MaxDepth, StimulationKind) -> VNG ! {stimulate, Value, Stimulation, MaxDepth, StimulationKind}.



get_excitation(VNG) -> 
    VNG ! {get_excitation, self()}, 
    receive 
        {vns_excitation, VNsExcitation} -> VNsExcitation
    end.


reset_excitation(VNG) -> VNG ! reset_excitation.


get_neighbours(VNG, Value) -> 
    VNG ! {get_neighbours, Value, self()},
    receive
        {neighbours, Neighbours} -> Neighbours
    end.


delete(VNG) -> VNG ! delete.


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

init(categorical, VNGName, no_epsilon, GlobalCfg) ->
    report_vng_creation(categorical, VNGName, GlobalCfg),
    process_events(#state{vng_type=categorical, vng_name=VNGName, vns=#{}, min_value=na, max_value=na, all_vns_set=sets:new(), global_cfg=GlobalCfg});

init(numerical, VNGName, Epsilon, GlobalCfg) ->
    report_vng_creation(numerical, VNGName, GlobalCfg),
    process_events(#state{vng_type=numerical, vng_name=VNGName, vns=avb_tree:create(Epsilon), min_value=none, max_value=none, all_vns_set=sets:new(), global_cfg=GlobalCfg}).


report_vng_creation(VNGType, VNGName, #global_cfg{reporter=Reporter}) ->
    report:node_group_creation(self(), VNGName, {vng, VNGType}, Reporter).


% Separate AllVNsSet is stored to quicken sending messages to all VNs within VNG (could be replaced with pg:)
process_events(#state{vng_type=VNGType, vng_name=VNGName, vns=VNs, min_value=MinValue, max_value=MaxValue, all_vns_set=AllVNsSet, global_cfg=#global_cfg{reporter=Reporter} = GlobalCfg} = State) ->
    receive
        {add_value, AddedValue, RespectiveON} ->
            case VNGType of
                categorical -> 
                    case maps:find(AddedValue, VNs) of
                        {ok, VN} -> NewVNs = VNs;
                        error -> 
                            VN = vn:create_VN(AddedValue, categorical, VNGName, self(), GlobalCfg),
                            NewVNs = VNs#{AddedValue => VN}
                    end,

                    NewMinValue = MinValue, NewMaxValue = MaxValue;
                
                numerical -> 
                    if
                        MinValue == none, MaxValue == none -> NewMinValue = AddedValue, NewMaxValue = AddedValue;
                        AddedValue < MinValue -> NewMinValue = AddedValue, NewMaxValue = MaxValue, update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue);
                        AddedValue > MaxValue -> NewMinValue = MinValue, NewMaxValue = AddedValue, update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue);
                        true -> NewMinValue = MinValue, NewMaxValue = MaxValue
                    end,

                    {NewVNs, VN} = avb_tree:add(VNs, AddedValue, fun() -> vn:create_VN(AddedValue, vng_range(NewMinValue, NewMaxValue), VNGName, self(), GlobalCfg) end),
                    
                    Neighs = avb_tree:get_neighbours(NewVNs, AddedValue),

                    lists:foreach(
                        fun(Neigh) -> case Neigh of
                            none -> ok;
                            {NeighValue, NeighVN} ->
                                vn:connect_VN(VN, NeighVN, NeighValue),
                                vn:connect_VN(NeighVN, VN, AddedValue),
                                report:connection_formed(VN, NeighVN, Reporter)
                            end
                        end, 
                        tuple_to_list(Neighs)
                    )
            end,

            vn:connect_ON(VN, RespectiveON),
            on:connect_VN(RespectiveON, VN),
            report:connection_formed(VN, RespectiveON, Reporter),

            NewAllVNsSet = sets:add_element(VN, AllVNsSet),

            process_events(State#state{vns=NewVNs, min_value=NewMinValue, max_value=NewMaxValue, all_vns_set=NewAllVNsSet});


        {stimulate, all, repr_value, MaxDepth, StimulationKind} ->
            Scaler = case vng_range(MinValue, MaxValue) of
                0.0 -> 1.0; 
                VNGRange -> VNGRange
            end,

            case VNGType of
                categorical -> lists:foreach(fun({Value, VN}) -> vn:stimulate(VN, self(), Value / Scaler, 0, MaxDepth, StimulationKind) end, maps:to_list(VNs));
                numerical -> avb_tree:foreach(fun(Value, VN) -> vn:stimulate(VN, self(), Value / Scaler, 0, MaxDepth, StimulationKind) end, VNs)
            end,

            process_events(State);

        {stimulate, Value, Stimulation, MaxDepth, StimulationKind} ->
            case VNGType of
                categorical ->
                    case maps:find(Value, VNs) of
                        {ok, VN} -> vn:stimulate(VN, self(), Stimulation, 0, MaxDepth, StimulationKind);
                        error -> ok
                    end;

                numerical ->
                    case avb_tree:get_nearest(VNs, Value) of
                        {exact_match, VN} -> vn:stimulate(VN, self(), Stimulation, 0, MaxDepth, StimulationKind);
                        {none, none} -> ok;
                        {{LeftVNValue, LeftVN}, {RightVNValue, RightVN}} ->
                            vn:stimulate(LeftVN, self(), get_nearby_VN_stimuli(Value, LeftVNValue, MaxValue - MinValue, Stimulation), 0, MaxDepth, StimulationKind),
                            vn:stimulate(RightVN, self(), get_nearby_VN_stimuli(Value, RightVNValue, MaxValue - MinValue, Stimulation), 0, MaxDepth, StimulationKind)
                    end
            end,

            process_events(State);


        {get_excitation, Caller} ->
            VNsExcitation = case VNGType of
                categorical -> [{ReprValue, vn:get_excitation(VN)} || {ReprValue, VN} <- maps:to_list(VNs)];
                numerical -> [{ReprValue, vn:get_excitation(VN)} || {ReprValue, VN, _Occurances} <- avb_tree:items(VNs)]
            end,

            Caller ! {vns_excitation, VNsExcitation},
            process_events(State);


        reset_excitation ->
            case VNGType of
                categorical -> maps:foreach(fun(_Value, VN) -> vn:reset_excitation(VN) end, VNs);
                numerical -> avb_tree:foreach(fun(_Value, VN) -> vn:reset_excitation(VN) end, VNs)
            end,
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


        delete ->
            case VNGType of
                categorical -> maps:foreach(fun(_Value, VN) -> vn:delete(VN) end, VNs);
                numerical -> avb_tree:foreach(fun(_Value, VN) -> vn:delete(VN) end, VNs)
            end

    end.


vng_range(none, none) -> 0.0;

vng_range(MinValue, MaxValue) -> MaxValue - MinValue.


update_VNG_range(AllVNsSet, NewMinValue, NewMaxValue) ->
    lists:foreach(fun(VN) -> vn:update_VNG_range(VN, vng_range(NewMinValue, NewMaxValue)) end, sets:to_list(AllVNsSet)).


get_nearby_VN_stimuli(ExactValue, VNReprValue, VNGRange, Stimulation) ->  Stimulation * (1 - abs(ExactValue - VNReprValue) / VNGRange).
