-module(avb_tree).
-export([create/1, add/3, get/2, get_neighbours/2, get_nearest/2, get_min/1, get_max/1, foreach/2, items/1]).

-record(tree, {root, epsilon}).



%% %%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%

create(Epsilon) -> #tree{root=no_node, epsilon=Epsilon}.


%%% returns modified tree, {new_value, NewValue} | {existing_value, ExistingValue}
add(#tree{root=no_node} = Tree, NewKey, NewValueConstructor) -> 
    NewValue = NewValueConstructor(),
    NewRoot = {no_node, {NewKey, NewValue, 1}, no_node, no_content, no_node},
    {Tree#tree{root=NewRoot}, {new_value, NewValue}};
    
add(#tree{root=Root, epsilon=Epsilon} = Tree, NewKey, NewValueConstructor) -> 
    case insert_down(Root, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewRoot, InsertedValue} -> {Tree#tree{root=NewRoot}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> 
            NewRoot = {PassedLeftSubtree, PassedContent, PassedRightSubtree, no_content, no_node},
            {Tree#tree{root=NewRoot}, InsertedValue}
    end.


get(#tree{root=Root, epsilon=Epsilon}, Key) -> 
    case get_content(Root, Key, Epsilon) of
        {_Key, Value, Occurances} -> {Value, Occurances};
        no_content -> none
    end.


%%% returns {neigh(), neigh()} where neigh(): {Key, Value} | none
get_neighbours(#tree{root=Root, epsilon=Epsilon}, Key) -> {
    case get_left_neigh(Root, Key, no_content, Epsilon) of
        no_content -> none;
        {LeftKey, LeftValue, _LeftOccurances} -> {LeftKey, LeftValue}
    end,
    case get_right_neigh(Root, Key, no_content, Epsilon) of
        no_content -> none;
        {RightKey, RightValue, _RightOccurances} -> {RightKey, RightValue}
    end
}.


%%% returns {exact_match, Value} if Key is stored in AVBTree, nearest neighbours otherwise
get_nearest(#tree{root=Root, epsilon=Epsilon} = Tree, Key) ->
    case get_content(Root, Key, Epsilon) of
        no_content -> get_neighbours(Tree, Key);
        {_Key, Value, _Occurances} -> {exact_match, Value}
    end.


get_min(#tree{root=Root}) -> get_min_impl(Root).


get_max(#tree{root=Root}) -> get_max_impl(Root).


foreach(Fun, #tree{root=Root}) -> foreach_impl(Fun, Root).


items(#tree{root=Root}) -> items_impl(Root).


%% %%%%%%%%%%%%%%% Internals %%%%%%%%%%%%%%%

%% AVB Node: {LeftChild, {LeftKey, LeftValue, LeftOccurances} = LeftContent, MiddleChild, {RightKey, RightValue, RightOccurances} = RightContent, RightChild} | no_node
%% AVB Content: {Key, Value, Occurances} | no_content


%% insert_down: value already stored

insert_down({LeftChild, {LeftKey, LeftValue, LeftOccurances}, MiddleChild, RightValue, RightChild}, NewKey, _NewValueConstructor, Epsilon) when abs(LeftKey - NewKey) < Epsilon -> 
    {no_split, {LeftChild, {LeftKey, LeftValue, LeftOccurances + 1}, MiddleChild, RightValue, RightChild}, {existing_value, LeftValue}};

insert_down({LeftChild, LeftValue, MiddleChild, {RightKey, RightValue, RightOccurances}, RightChild}, NewKey, _NewValueConstructor, Epsilon) when abs(RightKey - NewKey) < Epsilon -> 
    {no_split, {LeftChild, LeftValue, MiddleChild, {RightKey, RightValue, RightOccurances + 1}, RightChild}, {existing_value, RightValue}};


%% insert_down: leaves

insert_down({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, no_content, no_node}, NewKey, NewValueConstructor, _Epsilon) when NewKey < LeftKey -> 
    NewValue = NewValueConstructor(),
    {no_split, {no_node, {NewKey, NewValue, 1}, no_node, LeftContent, no_node}, {new_value, NewValue}};

insert_down({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, no_content, no_node}, NewKey, NewValueConstructor, _Epsilon) when NewKey > LeftKey -> 
    NewValue = NewValueConstructor(),
    {no_split, {no_node, LeftContent, no_node, {NewKey, NewValue, 1}, no_node}, {new_value, NewValue}};
    
insert_down({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, RightContent, no_node}, NewKey, NewValueConstructor, _Epsilon) when NewKey < LeftKey -> 
    NewValue = NewValueConstructor(),
    {split, {no_node, {NewKey, NewValue, 1}, no_node, no_content, no_node}, LeftContent, {no_node, RightContent, no_node, no_content, no_node}, {new_value, NewValue}};

insert_down({no_node, LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, NewKey, NewValueConstructor, _Epsilon) when NewKey < RightKey -> 
    NewValue = NewValueConstructor(),
    {split, {no_node, LeftContent, no_node, no_content, no_node}, {NewKey, NewValue, 1}, {no_node, RightContent, no_node, no_content, no_node}, {new_value, NewValue}};

insert_down({no_node, LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, NewKey, NewValueConstructor, _Epsilon) when NewKey > RightKey -> 
    NewValue = NewValueConstructor(),
    {split, {no_node, LeftContent, no_node, no_content, no_node}, RightContent, {no_node, {NewKey, NewValue, 1}, no_node, no_content, no_node}, {new_value, NewValue}};


%% insert_down: middle nodes

insert_down({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, no_content, no_node}, NewKey, NewValueConstructor, Epsilon) when NewKey < LeftKey ->
    case insert_down(LeftChild, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewSubtree, InsertedValue} -> {no_split, {NewSubtree, LeftContent, MiddleChild, no_content, no_node}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> {no_split, {PassedLeftSubtree, PassedContent, PassedRightSubtree, LeftContent, MiddleChild}, InsertedValue}
    end;

insert_down({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, no_content, no_node}, NewKey, NewValueConstructor, Epsilon) when NewKey > LeftKey ->
    case insert_down(MiddleChild, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewSubtree, InsertedValue} -> {no_split, {LeftChild, LeftContent, NewSubtree, no_content, no_node}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> {no_split, {LeftChild, LeftContent, PassedLeftSubtree, PassedContent, PassedRightSubtree}, InsertedValue}
    end;

insert_down({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, RightContent, RightChild}, NewKey, NewValueConstructor, Epsilon) when NewKey < LeftKey ->
    case insert_down(LeftChild, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewSubtree, InsertedValue} -> {no_split, {NewSubtree, LeftContent, MiddleChild, RightContent, RightChild}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> 
            {
                split, 
                {PassedLeftSubtree, PassedContent, PassedRightSubtree, no_content, no_node}, 
                LeftContent, 
                {MiddleChild, RightContent, RightChild, no_content, no_node}, 
                InsertedValue
            }
    end;

insert_down({LeftChild, LeftContent, MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, RightChild}, NewKey, NewValueConstructor, Epsilon) when NewKey < RightKey ->
    case insert_down(MiddleChild, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewSubtree, InsertedValue} -> {no_split, {LeftChild, LeftContent, NewSubtree, RightContent, RightChild}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> 
            {
                split,
                {LeftChild, LeftContent, PassedLeftSubtree, no_content, no_node}, 
                PassedContent, 
                {PassedRightSubtree, RightContent, RightChild, no_content, no_node}, 
                InsertedValue
            }
    end;

insert_down({LeftChild, LeftContent, MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, RightChild}, NewKey, NewValueConstructor, Epsilon) when NewKey > RightKey ->
    case insert_down(RightChild, NewKey, NewValueConstructor, Epsilon) of
        {no_split, NewSubtree, InsertedValue} -> {no_split, {LeftChild, LeftContent, MiddleChild, RightContent, NewSubtree}, InsertedValue};
        {split, PassedLeftSubtree, PassedContent, PassedRightSubtree, InsertedValue} -> 
            {
                split,
                {LeftChild, LeftContent, MiddleChild, no_content, no_node}, 
                RightContent, 
                {PassedLeftSubtree, PassedContent, PassedRightSubtree, no_content, no_node}, 
                InsertedValue
            }
    end.



get_content(no_node, _SearchedKey, _Epsilon) -> no_content;

get_content({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, _MiddleChild, _RightContent, _RightChild}, SearchedKey, Epsilon) when abs(SearchedKey - LeftKey) < Epsilon -> LeftContent;

get_content({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, _RightChild}, SearchedKey, Epsilon) when abs(SearchedKey - RightKey) < Epsilon -> RightContent;

get_content({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, Epsilon) when abs(SearchedKey - LeftKey) >= Epsilon, abs(SearchedKey - RightKey) >= Epsilon -> no_content;

get_content({LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, _MiddleChild, _RightContent, _RightChild}, SearchedKey, Epsilon) when SearchedKey < LeftKey -> get_content(LeftChild, SearchedKey, Epsilon);

get_content({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, no_content, no_node}, SearchedKey, Epsilon) when SearchedKey > LeftKey -> get_content(MiddleChild, SearchedKey, Epsilon);

get_content({_LeftChild, _LeftContent, MiddleChild, {RightKey, _RightValue, _RightOccurances}, _RightChild}, SearchedKey, Epsilon) when SearchedKey < RightKey -> get_content(MiddleChild, SearchedKey, Epsilon);

get_content({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances}, RightChild}, SearchedKey, Epsilon) when SearchedKey > RightKey -> get_content(RightChild, SearchedKey, Epsilon).


% get_left_neigh: leaves, CurrentNeighCandidate == no_content
get_left_neigh(no_node, _SearchedKey, no_content, _Epsilon) -> no_content;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, _RightContent, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey < LeftKey + Epsilon ->
        no_content;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, no_content, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon ->
        LeftContent;

get_left_neigh({no_node, LeftContent, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey < RightKey + Epsilon ->
        LeftContent;

get_left_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey >= RightKey + Epsilon ->
        RightContent;

% get_left_neigh: leaves, CurrentNeighCandidate /= no_content
get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = _LeftContent, no_node, _RightContent, no_node}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey < LeftKey + Epsilon ->
        CurrentNeighCandidate;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon, CurrNeighKey < LeftKey ->
        LeftContent;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon, CurrNeighKey > LeftKey ->
        CurrentNeighCandidate;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = _CurrentNeighCandidate, Epsilon) 
    when SearchedKey < RightKey + Epsilon, CurrNeighKey < LeftKey ->
        LeftContent;

get_left_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey < RightKey + Epsilon, CurrNeighKey > LeftKey ->
        CurrentNeighCandidate;

get_left_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey >= RightKey + Epsilon, CurrNeighKey < RightKey ->
        RightContent;

get_left_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey >= RightKey + Epsilon, CurrNeighKey > RightKey ->
        CurrentNeighCandidate;


get_left_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, no_content, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon ->
        get_left_neigh(MiddleChild, SearchedKey, LeftContent, Epsilon);

get_left_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = _CurrentNeighCandidate, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon, CurrNeighKey < LeftKey ->
        get_left_neigh(MiddleChild, SearchedKey, LeftContent, Epsilon);

get_left_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey >= LeftKey + Epsilon, CurrNeighKey > LeftKey ->
        get_left_neigh(MiddleChild, SearchedKey, CurrentNeighCandidate, Epsilon);

get_left_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, _MiddleChild, _RightContent, _RightChild}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey < LeftKey + Epsilon ->
        get_left_neigh(LeftChild, SearchedKey, CurrentNeighCandidate, Epsilon);


get_left_neigh({_LeftChild, LeftContent, MiddleChild, {RightKey, _RightValue, _RightOccurances}, _RightChild}, SearchedKey, no_content, Epsilon) 
    when SearchedKey < RightKey + Epsilon ->
        get_left_neigh(MiddleChild, SearchedKey, LeftContent, Epsilon);

get_left_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, MiddleChild, {RightKey, _RightValue, _RightOccurances}, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey < RightKey + Epsilon, CurrNeighKey < LeftKey ->
        get_left_neigh(MiddleChild, SearchedKey, LeftContent, Epsilon);

get_left_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, {RightKey, _RightValue, _RightOccurances}, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey < RightKey + Epsilon, CurrNeighKey > LeftKey ->
        get_left_neigh(MiddleChild, SearchedKey, CurrentNeighCandidate, Epsilon);


get_left_neigh({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, RightChild}, SearchedKey, no_content, Epsilon) 
    when SearchedKey >= RightKey + Epsilon ->
        get_left_neigh(RightChild, SearchedKey, RightContent, Epsilon);

get_left_neigh({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey >= RightKey + Epsilon, CurrNeighKey < RightKey ->
        get_left_neigh(RightChild, SearchedKey, RightContent, Epsilon);

get_left_neigh({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances}, RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey >= RightKey + Epsilon, CurrNeighKey > RightKey ->
        get_left_neigh(RightChild, SearchedKey, CurrentNeighCandidate, Epsilon).



% get_right_neigh: empty tree

get_right_neigh(no_node, _SearchedKey, no_content, _Epsilon) -> no_content;


% get_right_neigh: leaves, CurrentNeighCandidate == no_content

get_right_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, no_content, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey > LeftKey - Epsilon ->
        no_content;

get_right_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, _RightContent, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon ->
        LeftContent;

get_right_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey =< RightKey - Epsilon ->
        RightContent;

get_right_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey > RightKey - Epsilon ->
        no_content;


% get_right_neigh: leaves, CurrentNeighCandidate /= no_content
get_right_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, _RightContent, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey < LeftKey ->
        CurrentNeighCandidate;

get_right_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, no_node, _RightContent, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey > LeftKey ->
        LeftContent;

get_right_neigh({no_node, {LeftKey, _LeftValue, _LeftOccurances}, no_node, no_content, no_node}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey > LeftKey - Epsilon ->
        CurrentNeighCandidate;

get_right_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances}, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey =< RightKey - Epsilon, CurrNeighKey < RightKey ->
        CurrentNeighCandidate;

get_right_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = RightContent, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey =< RightKey - Epsilon, CurrNeighKey > RightKey ->
        RightContent;

get_right_neigh({no_node, _LeftContent, no_node, {RightKey, _RightValue, _RightOccurances} = _RightContent, no_node}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey > RightKey - Epsilon ->
        CurrentNeighCandidate;


% get_right_neigh: middle nodes, single content element (= two children)
get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, _MiddleChild, no_content, no_node}, SearchedKey, no_content, Epsilon) 
    when SearchedKey < LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, LeftContent, Epsilon);

get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, _MiddleChild, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey > LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, LeftContent, Epsilon);

get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, _MiddleChild, no_content, no_node}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey < LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, CurrentNeighCandidate, Epsilon);

get_right_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, no_content, _RightChild}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey > LeftKey - Epsilon ->
        get_right_neigh(MiddleChild, SearchedKey, CurrentNeighCandidate, Epsilon);


% get_right_neigh: middle nodes, two content elements (= three children)
get_right_neigh({_LeftChild, _LeftContent, _MiddleChild, {RightKey, _RightValue, _RightOccurances}, RightChild}, SearchedKey, CurrentNeighCandidate, Epsilon) 
    when SearchedKey > RightKey - Epsilon ->
        get_right_neigh(RightChild, SearchedKey, CurrentNeighCandidate, Epsilon);

get_right_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, RightContent, _RightChild}, SearchedKey, no_content, Epsilon) 
    when SearchedKey > LeftKey - Epsilon ->
        get_right_neigh(MiddleChild, SearchedKey, RightContent, Epsilon);

get_right_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, {RightKey, _RightValue, _RightOccurances}, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey > LeftKey - Epsilon, CurrNeighKey < RightKey ->
        get_right_neigh(MiddleChild, SearchedKey, CurrentNeighCandidate, Epsilon);

get_right_neigh({_LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, MiddleChild, {RightKey, _RightValue, _RightOccurances} = RightContent, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey > LeftKey - Epsilon, CurrNeighKey > RightKey ->
        get_right_neigh(MiddleChild, SearchedKey, RightContent, Epsilon);

get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, _MiddleChild, _RightContent, _RightChild}, SearchedKey, no_content, Epsilon) 
    when SearchedKey < LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, LeftContent, Epsilon);

get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances} = LeftContent, _MiddleChild, _RightContent, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc}, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey > LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, LeftContent, Epsilon);

get_right_neigh({LeftChild, {LeftKey, _LeftValue, _LeftOccurances}, _MiddleChild, _RightContent, _RightChild}, SearchedKey, {CurrNeighKey, _CurrNeighValue, _CurrNeighOcc} = CurrentNeighCandidate, Epsilon) 
    when SearchedKey =< LeftKey - Epsilon, CurrNeighKey < LeftKey ->
        get_right_neigh(LeftChild, SearchedKey, CurrentNeighCandidate, Epsilon).



get_min_impl(no_node) -> no_content;

get_min_impl({no_node, LeftContent, no_node, _RightContent, no_node}) -> LeftContent;

get_min_impl({LeftChild, _LeftContent, _MiddleChild, _RightContent, _RightChild}) -> get_min_impl(LeftChild).


get_max_impl(no_node) -> no_content;

get_max_impl({no_node, LeftContent, no_node, no_content, no_node}) -> LeftContent;

get_max_impl({no_node, _LeftContent, no_node, RightContent, no_node}) -> RightContent;

get_max_impl({_LeftChild, _LeftContent, MiddleChild, no_content, no_node}) -> get_max_impl(MiddleChild);

get_max_impl({_LeftChild, _LeftContent, _MiddleChild, _RightContent, RightChild}) -> get_max_impl(RightChild).



foreach_impl(_Fun, no_node) -> ok;

foreach_impl(Fun, {LeftChild, LeftContent, MiddleChild, no_content, no_child}) ->
    foreach_impl(Fun, LeftChild), 
    execute_if_content_present(Fun, LeftContent), 
    foreach_impl(Fun, MiddleChild);

foreach_impl(Fun, {LeftChild, LeftContent, MiddleChild, RightContent, RightChild}) ->
    foreach_impl(Fun, LeftChild), 
    execute_if_content_present(Fun, LeftContent), 
    foreach_impl(Fun, MiddleChild), 
    execute_if_content_present(Fun, RightContent), 
    foreach_impl(Fun, RightChild).


execute_if_content_present(_Fun, no_content) -> ok;

execute_if_content_present(Fun, {Key, Value, _Occurances}) -> Fun(Key, Value).



items_impl(no_node) -> [];

items_impl({LeftChild, LeftContent, MiddleChild, RightContent, RightChild}) -> 
    items_impl(LeftChild) 
    ++ item_if_content_present(LeftContent)
    ++ items_impl(MiddleChild)
    ++ item_if_content_present(RightContent)
    ++ items_impl(RightChild).


item_if_content_present(no_content) -> [];

item_if_content_present(Content) -> [Content].
