-module(avb_tree_tests).
-include_lib("eunit/include/eunit.hrl").

-record(tree, {root, epsilon}).


% ****************************** BUILD TREE TESTS ******************************

build_test_() ->
    {
        "Tests inserting elements into an AVB+ tree",
        {
            setup,
            fun create_tree/0,
            fun (AVBTree) -> [
                test_tree_is_empty(AVBTree),
                test_adding_keys_to_leaf(AVBTree),
                test_splitting_child_nodes()
            ] end
        }
    }.



create_tree() -> avb_tree:create(0.01).



test_tree_is_empty(AVBTree) -> ?_assertMatch(#tree{root=no_node}, AVBTree).




test_adding_keys_to_leaf(AVBTree) ->
    [
        test_leaf_add_single_key(AVBTree),
        test_leaf_add_second_smaller_key(AVBTree),
        test_leaf_add_second_equal_key_with_same_value(AVBTree),
        test_leaf_add_second_equal_key_with_different_value(AVBTree),
        test_leaf_add_second_bigger_key(AVBTree),
        test_leaf_add_third_key_already_present(AVBTree),
        test_leaf_add_third_key_left_new_root(AVBTree),
        test_leaf_add_third_key_middle_new_root(AVBTree),
        test_leaf_add_third_key_right_new_root(AVBTree)
    ].


test_leaf_add_single_key(AVBTree) -> ?_assertMatch({#tree{root={no_node, {1, stored_value, 1}, no_node, no_content, no_node}}, {new_value, stored_value}}, avb_tree:add(AVBTree, 1, fun() -> stored_value end)).

test_leaf_add_second_smaller_key(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_0}} = avb_tree:add(AVBTree1, 0, fun() -> stored_value_0 end),
    ?_assertMatch(#tree{root={no_node, {0, stored_value_0, 1}, no_node, {1, stored_value_1, 1}, no_node}}, AVBTree2).

test_leaf_add_second_equal_key_with_same_value(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {existing_value, stored_value_1}} = avb_tree:add(AVBTree1, 1, fun() -> stored_value_1 end),
    ?_assertMatch(#tree{root={no_node, {1, stored_value_1, 2}, no_node, no_content, no_node}}, AVBTree2).

test_leaf_add_second_equal_key_with_different_value(AVBTree) ->
    % new value should be ignored
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {existing_value, stored_value_1}} = avb_tree:add(AVBTree1, 1, fun() -> stored_value_diff end),
    ?_assertMatch(#tree{root={no_node, {1, stored_value_1, 2}, no_node, no_content, no_node}}, AVBTree2).

test_leaf_add_second_bigger_key(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_2}} = avb_tree:add(AVBTree1, 2, fun() -> stored_value_2 end),
    ?_assertMatch(#tree{root={no_node, {1, stored_value_1, 1}, no_node, {2, stored_value_2, 1}, no_node}}, AVBTree2).

test_leaf_add_third_key_already_present(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_2}} = avb_tree:add(AVBTree1, 2, fun() -> stored_value_2 end),
    {AVBTree3, {existing_value, stored_value_2}} = avb_tree:add(AVBTree2, 2, fun() -> stored_value_2 end),
    ?_assertMatch(#tree{root={no_node, {1, stored_value_1, 1}, no_node, {2, stored_value_2, 2}, no_node}}, AVBTree3).

test_leaf_add_third_key_left_new_root(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_2}} = avb_tree:add(AVBTree1, 2, fun() -> stored_value_2 end),
    {AVBTree3, {new_value, stored_value_0}} = avb_tree:add(AVBTree2, 0, fun() -> stored_value_0 end),
    ?_assertMatch(#tree{root={
        {no_node, {0, stored_value_0, 1}, no_node, no_content, no_node}, 
        {1, stored_value_1, 1}, 
        {no_node, {2, stored_value_2, 1}, no_node, no_content, no_node}, 
        no_content, 
        no_node
    }}, AVBTree3).

test_leaf_add_third_key_middle_new_root(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_2}} = avb_tree:add(AVBTree1, 2, fun() -> stored_value_2 end),
    {AVBTree3, {new_value, stored_value_1_5}} = avb_tree:add(AVBTree2, 1.5, fun() -> stored_value_1_5 end),
    ?_assertMatch(#tree{root={
        {no_node, {1, stored_value_1, 1}, no_node, no_content, no_node}, 
        {1.5, stored_value_1_5, 1}, 
        {no_node, {2, stored_value_2, 1}, no_node, no_content, no_node}, 
        no_content, 
        no_node
    }}, AVBTree3).

test_leaf_add_third_key_right_new_root(AVBTree) ->
    {AVBTree1, {new_value, stored_value_1}} = avb_tree:add(AVBTree, 1, fun() -> stored_value_1 end),
    {AVBTree2, {new_value, stored_value_2}} = avb_tree:add(AVBTree1, 2, fun() -> stored_value_2 end),
    {AVBTree3, {new_value, stored_value_3}} = avb_tree:add(AVBTree2, 3, fun() -> stored_value_3 end),
    ?_assertMatch(#tree{root={
        {no_node, {1, stored_value_1, 1}, no_node, no_content, no_node}, 
        {2, stored_value_2, 1}, 
        {no_node, {3, stored_value_3, 1}, no_node, no_content, no_node}, 
        no_content, 
        no_node
    }}, AVBTree3).
    


test_splitting_child_nodes() ->
    {
        setup,
        fun sample_tree/0,
        fun (SampleAVBTree) -> [
            test_accept_new_second_value_left(SampleAVBTree),
            test_accept_new_second_value_right(SampleAVBTree),
            test_accept_new_third_value_left(SampleAVBTree),
            test_accept_new_third_value_middle(SampleAVBTree),
            test_accept_new_third_value_right(SampleAVBTree)
        ] end
    }.


test_accept_new_second_value_left(SampleAVBTree) ->
    StoredValue = sample_stored_value(10.5),
    {
        #tree{root={
            _LeftSubtree, 
            _LeftContent,
            MiddleSubtree,
            no_content,
            no_node
        }},
        {new_value, StoredValue}
    } = avb_tree:add(SampleAVBTree, 10.5, fun() -> StoredValue end),
    ?_assertEqual(
        {
            sample_leaf(9),
            sample_content(10),
            sample_leaf(10.5),
            sample_content(11),
            sample_leaf(12)
        },
        MiddleSubtree
    ).


test_accept_new_second_value_right(SampleAVBTree) ->
    {TreeAfterFirstInsertion, {new_value, _FirstStoredValue}} = avb_tree:add(SampleAVBTree, 12.5, fun() -> sample_stored_value(12.5) end),
    SecondStoredValue = sample_stored_value(13),
    {
        #tree{root={
            _LeftSubtree, 
            _LeftContent,
            MiddleSubtree,
            no_content,
            no_node
        }},
        {new_value, SecondStoredValue}
    } = avb_tree:add(TreeAfterFirstInsertion, 13, fun() -> SecondStoredValue end),
    ?_assertEqual(
        {
            sample_leaf(9, 10),
            sample_content(11),
            sample_leaf(12),
            sample_content(12.5),
            sample_leaf(13)
        },
        MiddleSubtree
    ).


test_accept_new_third_value_left(SampleAVBTree) ->
    StoredValue = sample_stored_value(-0.5),
    {
        #tree{root={
            LeftSubtree,
            LeftContent,
            MiddleSubtree,
            _RightContent,
            _RightSubtree
        }},
        {new_value, StoredValue}
    } = avb_tree:add(SampleAVBTree, -0.5, fun() -> StoredValue end),
    [
        ?_assertEqual(
            {
                sample_leaf(-0.5),
                sample_content(0),
                sample_leaf(1),
                no_content,
                no_node
            },
            LeftSubtree
        ),
        ?_assertEqual(sample_content(2), LeftContent),
        ?_assertEqual(
            {
                sample_leaf(3, 4),
                sample_content(5),
                sample_leaf(6, 7),
                no_content,
                no_node
            },
            MiddleSubtree
        )
    ].

test_accept_new_third_value_middle(SampleAVBTree) ->
    StoredValue = sample_stored_value(3.5),
    {
        #tree{root={
            LeftSubtree,
            LeftContent,
            MiddleSubtree,
            _RightContent,
            _RightSubtree
        }},
        {new_value, StoredValue}
    } = avb_tree:add(SampleAVBTree, 3.5, fun() -> StoredValue end),
    [
        ?_assertEqual(
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3),
                no_content,
                no_node
            },
            LeftSubtree
        ),
        ?_assertEqual(sample_content(3.5), LeftContent),
        ?_assertEqual(
            {
                sample_leaf(4),
                sample_content(5),
                sample_leaf(6, 7),
                no_content,
                no_node
            },
            MiddleSubtree
        )
    ].

test_accept_new_third_value_right(SampleAVBTree) ->
    StoredValue = sample_stored_value(5.5),
    {
        #tree{root={
                LeftSubtree,
                LeftContent,
                MiddleSubtree,
                _RightContent,
                _RightSubtree
            }},
            {new_value, StoredValue}
    } = avb_tree:add(SampleAVBTree, 5.5, fun() -> StoredValue end),
    [
        ?_assertEqual(
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3, 4),
                no_content,
                no_node
            },
            LeftSubtree
        ),
        ?_assertEqual(sample_content(5), LeftContent),
        ?_assertEqual(
            {
                sample_leaf(5.5),
                sample_content(6),
                sample_leaf(7),
                no_content,
                no_node
            },
            MiddleSubtree
        )
    ].


% ******************************** DELETE TESTS *********************************

delete_test_() ->
    {
        "Tests deleting elements from an AVB+ tree",
        {
            setup,
            fun sample_tree/0,
            fun (SampleAVBTree) -> [
                test_delete_non_existant_key(SampleAVBTree),
                test_delete_from_single_valued_leaf(SampleAVBTree),
                test_delete_left_value_from_two_valued_leaf(SampleAVBTree),
                test_delete_right_value_from_two_valued_leaf(SampleAVBTree),
                test_delete_from_single_valued_middle_node(SampleAVBTree),
                test_delete_left_value_from_two_valued_middle_node(SampleAVBTree),
                test_delete_right_value_from_two_valued_middle_node(SampleAVBTree),
                test_delete_from_single_valued_root(SampleAVBTree)
            ] end
        }
    }.


test_delete_non_existant_key(AVBTree) -> ?_assertEqual(AVBTree, avb_tree:delete(AVBTree, -1)).


test_delete_from_single_valued_leaf(AVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(AVBTree, 12),
    ?_assertEqual(
        {
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3, 4),
                sample_content(5),
                sample_leaf(6, 7)
            },
            sample_content(8),
            {
                sample_leaf(9),
                sample_content(10),
                sample_leaf(11),
                no_content,
                no_node
            },
            no_content,
            no_node
        },
        AVBTreeAfterDeletion#tree.root).


test_delete_left_value_from_two_valued_leaf(AVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(AVBTree, 3),
    ?_assertEqual(
        {
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(4),
                sample_content(5),
                sample_leaf(6, 7)
            },
            sample_content(8),
            {
                sample_leaf(9, 10),
                sample_content(11),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        }, 
        AVBTreeAfterDeletion#tree.root).


test_delete_right_value_from_two_valued_leaf(AVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(AVBTree, 7),
    ?_assertEqual(
        {
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3, 4),
                sample_content(5),
                sample_leaf(7)
            },
            sample_content(8),
            {
                sample_leaf(9, 10),
                sample_content(11),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        }, 
        AVBTreeAfterDeletion#tree.root).


test_delete_from_single_valued_middle_node(SampleAVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(SampleAVBTree, 11),
    ?_assertEqual(
        {
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3, 4),
                sample_content(5),
                sample_leaf(6, 7)
            },
            sample_content(8),
            {
                sample_leaf(9),
                sample_content(10),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        }, 
        AVBTreeAfterDeletion#tree.root).


test_delete_left_value_from_two_valued_middle_node(SampleAVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(SampleAVBTree, 2),
    ?_assertEqual(
        {
            {
                {
                    {
                        no_node,
                        sample_content(0),
                        no_node,
                        no_content,
                        no_node
                    },
                    sample_content(1),
                    {
                        no_node,
                        sample_content(3),
                        no_node,
                        no_content,
                        no_node
                    },
                    no_content,
                    no_node
                },
                sample_content(4),
                {
                    {
                        no_node,
                        sample_content(5),
                        no_node,
                        no_content,
                        no_node
                    },
                    sample_content(6),
                    {
                        no_node,
                        sample_content(7),
                        no_node,
                        no_content,
                        no_node
                    },
                    no_content,
                    no_node
                },
                no_content,
                no_node
            },
            sample_content(8),
            {
                sample_leaf(9, 10),
                sample_content(11),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        }, 
        AVBTreeAfterDeletion#tree.root).


test_delete_right_value_from_two_valued_middle_node(SampleAVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(SampleAVBTree, 5),
    ?_assertEqual(
        {
            {
                {
                    {
                        no_node,
                        sample_content(0),
                        no_node,
                        no_content,
                        no_node
                    },
                    sample_content(1),
                    {
                        no_node,
                        sample_content(2),
                        no_node,
                        no_content,
                        no_node
                    },
                    no_content,
                    no_node
                },
                sample_content(3),
                {
                    {
                        no_node,
                        sample_content(4),
                        no_node,
                        no_content,
                        no_node
                    },
                    sample_content(6),
                    {
                        no_node,
                        sample_content(7),
                        no_node,
                        no_content,
                        no_node
                    },
                    no_content,
                    no_node
                },
                no_content,
                no_node
            },
            sample_content(8),
            {
                sample_leaf(9, 10),
                sample_content(11),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        }, 
        AVBTreeAfterDeletion#tree.root).


test_delete_from_single_valued_root(SampleAVBTree) ->
    AVBTreeAfterDeletion = avb_tree:delete(SampleAVBTree, 8),
    ?_assertEqual(
    {
        {
            {
                no_node,
                sample_content(0),
                no_node,
                no_content,
                no_node
            },
            sample_content(1),
            {
                no_node,
                sample_content(2),
                no_node,
                no_content,
                no_node
            },
            no_content,
            no_node
        },
        sample_content(3),
        {
            {
                no_node,
                sample_content(4),
                no_node,
                no_content,
                no_node
            },
            sample_content(5),
            {
                no_node,
                sample_content(6),
                no_node,
                no_content,
                no_node
            },
            no_content,
            no_node
        },
        sample_content(7),
        {
            {
                no_node,
                sample_content(9),
                no_node,
                no_content,
                no_node
            },
            sample_content(10),
            {
                no_node,
                sample_content(11),
                no_node,
                sample_content(12),
                no_node
            },
            no_content,
            no_node
        }
    }, 
    AVBTreeAfterDeletion#tree.root).



% ****************************** GET CONTENT TESTS ******************************

get_content_test_() ->
    {
        "Tests getting stored value and number of occurances for given key",
        [
            test_get_content_empty_tree(),
            {
                setup,
                fun sample_tree/0,
                fun (SampleAVBTree) -> [
                    test_get_content_root(SampleAVBTree),
                    test_get_content_middle_node(SampleAVBTree),
                    test_get_content_leaf(SampleAVBTree),
                    test_get_content_not_present(SampleAVBTree)
                ] end
            }
        ]
    }.


test_get_content_empty_tree() -> test_get_content_generic_non_existing_key(create_tree(), 0).

test_get_content_not_present(SampleAVBTree) -> test_get_content_generic_non_existing_key(SampleAVBTree, -1).


test_get_content_root(SampleAVBTree) -> test_get_content_generic_existing_key(SampleAVBTree, 8).

test_get_content_middle_node(SampleAVBTree) -> test_get_content_generic_existing_key(SampleAVBTree, 5).
        
test_get_content_leaf(SampleAVBTree) -> test_get_content_generic_existing_key(SampleAVBTree, 10).



test_get_content_generic_existing_key(AVBTree, Key) ->
    {_Key, Value, Occurances} = sample_content(Key),
    ?_assertEqual({Value, Occurances}, avb_tree:get(AVBTree, Key)).


test_get_content_generic_non_existing_key(AVBTree, Key) ->
    ?_assertEqual(none, avb_tree:get(AVBTree, Key)).


 
% ****************************** GET NEIGHBOURS TESTS ******************************

get_neighbours_test_() ->
    {
        "Tests getting left and right neighbour of a given key",
        [
            test_get_neighbours_empty_tree(),
            test_get_neighbours_no_neighs(),
            {
                setup,
                fun sample_tree/0,
                fun (SampleAVBTree) -> [
                    test_get_neighbours_non_existant_key(SampleAVBTree),
                    test_get_neighbours_no_left_neigh(SampleAVBTree),
                    test_get_neighbours_no_right_neigh(SampleAVBTree),
                    test_get_neighbours_key_in_root(SampleAVBTree),
                    test_get_neighbours_key_in_middle_node(SampleAVBTree),
                    test_get_neighbours_key_in_leaf(SampleAVBTree)
                ] end
            }
        ]
    }.

test_get_neighbours_empty_tree() ->
    ?_assertEqual({none, none}, avb_tree:get_neighbours(avb_tree:create(0.1), 5)).

test_get_neighbours_no_neighs() ->
    EmptyTree = avb_tree:create(0.1),
    StoredValue = sample_stored_value(1),
    {SingleNodeTree, {new_value, StoredValue}} = avb_tree:add(EmptyTree, 1, fun() -> StoredValue end),
    ?_assertEqual({none, none}, avb_tree:get_neighbours(SingleNodeTree, 1)).

test_get_neighbours_non_existant_key(SampleAVBTree) ->
    ?_assertEqual({{2, sample_stored_value(2)}, {3, sample_stored_value(3)}}, avb_tree:get_neighbours(SampleAVBTree, 2.5)).

test_get_neighbours_no_left_neigh(SampleAVBTree) ->
    ?_assertEqual({none, {1, sample_stored_value(1)}}, avb_tree:get_neighbours(SampleAVBTree, 0)).

test_get_neighbours_no_right_neigh(SampleAVBTree) ->
    ?_assertEqual({{12, sample_stored_value(12)}, none}, avb_tree:get_neighbours(SampleAVBTree, 15)).

test_get_neighbours_key_in_root(SampleAVBTree) ->
    ?_assertEqual({{7, sample_stored_value(7)}, {9, sample_stored_value(9)}}, avb_tree:get_neighbours(SampleAVBTree, 8)).

test_get_neighbours_key_in_middle_node(SampleAVBTree) ->
    ?_assertEqual({{4, sample_stored_value(4)}, {6, sample_stored_value(6)}}, avb_tree:get_neighbours(SampleAVBTree, 5)).

test_get_neighbours_key_in_leaf(SampleAVBTree) ->
    ?_assertEqual({{6, sample_stored_value(6)}, {8, sample_stored_value(8)}}, avb_tree:get_neighbours(SampleAVBTree, 7

)).

% ****************************** GET MIN-MAX TESTS ******************************


get_min_max_test_() ->
    {
        "Tests getting minimum and maximum value from AVB+ tree",
        [
            test_get_minmax_empty_tree(),
            test_get_minmax_root_single_value(),
            test_get_minmax_root_double_value(),
            test_get_minmax_multilevel_tree()
        ]
    }.

test_get_minmax_empty_tree() -> test_get_minmax(create_tree(), no_content, no_content).

test_get_minmax_root_single_value() -> test_get_minmax(#tree{root=sample_leaf(1), epsilon=0.1}, sample_content(1), sample_content(1)).

test_get_minmax_root_double_value() -> test_get_minmax(#tree{root=sample_leaf(1, 2), epsilon=0.1}, sample_content(1), sample_content(2)).

test_get_minmax_multilevel_tree() -> test_get_minmax(sample_tree(), sample_content(0), sample_content(12)).



test_get_minmax(AVBTree, ExpectedMin, ExpectedMax) ->
    [
        ?_assertEqual(ExpectedMin, avb_tree:get_min(AVBTree)),
        ?_assertEqual(ExpectedMax, avb_tree:get_max(AVBTree))
    ].
    

% ****************************** HELPERS ******************************

sample_tree() -> 
    #tree{root = 
        {
            {
                sample_leaf(0, 1),
                sample_content(2),
                sample_leaf(3, 4),
                sample_content(5),
                sample_leaf(6, 7)
            },
            sample_content(8),
            {
                sample_leaf(9, 10),
                sample_content(11),
                sample_leaf(12),
                no_content,
                no_node
            },
            no_content,
            no_node
        },
        epsilon = 0.1
    }.


sample_leaf(LeftKey) -> 
    {
        no_node,
        sample_content(LeftKey),
        no_node,
        no_content,
        no_node
    }.

sample_leaf(LeftKey, RightKey) ->
    {
        no_node,
        sample_content(LeftKey),
        no_node,
        sample_content(RightKey),
        no_node
    }.

sample_content(Key) -> {Key, sample_stored_value(Key), 1}.

sample_stored_value(Key) -> {stored_value, Key}.
