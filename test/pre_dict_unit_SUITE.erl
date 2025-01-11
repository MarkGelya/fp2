-module(pre_dict_unit_SUITE).

-include_lib("../src/pre_dict.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    new_pre_dict_test/1,
    add_element_test/1,
    get_element_test/1,
    delete_element_test/1,
    map_test/1,
    fold_test/1,
    filter_test/1,
    equal_test/1,
    merge_test/1
]).

all() -> [
    add_element_test,
    new_pre_dict_test,
    add_element_test,
    get_element_test,
    delete_element_test,
    map_test,
    fold_test,
    filter_test,
    equal_test,
    merge_test
].

get_new_pre_dict() ->
    {pre_dict_node,#{},undefined,false}.

get_dataset() ->
    [
        % {"abc": 1}
        {pre_dict_node,#{97 =>
                     {pre_dict_node,#{98 =>
                                          {pre_dict_node,#{99 => {pre_dict_node,#{},1,true}},
                                                         undefined,false}},
                                    undefined,false}},
               undefined,false},
        % {"abc": 1, "acb": 2}
        {pre_dict_node,#{97 =>
                        {pre_dict_node,#{98 =>
                                            {pre_dict_node,#{99 => {pre_dict_node,#{},1,true}},
                                                            undefined,false},
                                        99 =>
                                            {pre_dict_node,#{98 => {pre_dict_node,#{},2,true}},
                                                            undefined,false}},
                                        undefined,false}},
                undefined,false},
        % {"abc": 1, "acb": 2, "abcd": 3}
        {pre_dict_node,#{97 =>
                     {pre_dict_node,#{98 =>
                                          {pre_dict_node,#{99 =>
                                                               {pre_dict_node,#{100 => {pre_dict_node,#{},3,true}},1,true}},
                                                         undefined,false},
                                      99 =>
                                          {pre_dict_node,#{98 => {pre_dict_node,#{},2,true}},
                                                         undefined,false}},
                                    undefined,false}},
               undefined,false},
        % {"abc": 1, "acb": 2, "abcd": 3, "ab": 4}
        {pre_dict_node,#{97 =>
                     {pre_dict_node,#{98 =>
                                          {pre_dict_node,#{99 =>
                                                               {pre_dict_node,#{100 => {pre_dict_node,#{},3,true}},1,true}},
                                                         4,true},
                                      99 =>
                                          {pre_dict_node,#{98 => {pre_dict_node,#{},2,true}},
                                                         undefined,false}},
                                    undefined,false}},
               undefined,false}
    ].

new_pre_dict_test(_) ->
    T0 = pre_dict:new(),
    ?assertEqual(T0, get_new_pre_dict()).

add_element_test(_) ->
    Dataset = get_dataset(),
    T0 = pre_dict:new(),
    T1 = pre_dict:insert("abc", 1, T0),
    ?assertEqual(T1, lists:nth(1, Dataset)),
    T2 = pre_dict:insert("acb", 2, T1),
    ?assertEqual(T2, lists:nth(2, Dataset)),
    T3 = pre_dict:insert("abcd", 3, T2),
    ?assertEqual(T3, lists:nth(3, Dataset)),
    T4 = pre_dict:insert("ab", 4, T3),
    ?assertEqual(T4, lists:nth(4, Dataset)).

get_element_test(_) ->
    Dataset = get_dataset(),
    T4 = lists:nth(4, Dataset),
    ?assertEqual(1, pre_dict:get("abc", T4)),
    ?assertEqual(2, pre_dict:get("acb", T4)),
    ?assertEqual(3, pre_dict:get("abcd", T4)),
    ?assertEqual(4, pre_dict:get("ab", T4)).

delete_element_test(_) ->
    Dataset = get_dataset(),
    T4 = lists:nth(4, Dataset),
    T3 = pre_dict:delete("ab", T4),
    ?assertEqual(T3, lists:nth(3, Dataset)),
    T2 = pre_dict:delete("abcd", T3),
    ?assertEqual(T2, lists:nth(2, Dataset)),
    T1 = pre_dict:delete("acb", T2),
    ?assertEqual(T1, lists:nth(1, Dataset)),
    T0 = pre_dict:delete("abc", T1),
    ?assertEqual(T0, get_new_pre_dict()).

map_test(_) ->
    Dataset = get_dataset(),
    T4 = lists:nth(4, Dataset),
    T4_new = pre_dict:map(fun({_, V}) -> V*2 end, T4),
    ?assertEqual(2, pre_dict:get("abc", T4_new)),
    ?assertEqual(4, pre_dict:get("acb", T4_new)),
    ?assertEqual(6, pre_dict:get("abcd", T4_new)),
    ?assertEqual(8, pre_dict:get("ab", T4_new)).

fold_test(_) ->
    Dataset = get_dataset(),
    T4 = lists:nth(4, Dataset),
    Res0 = pre_dict:fold(fun({_, V}, Acc) -> V + Acc end, 0, T4),
    ?assertEqual(10, Res0),
    Res1 = pre_dict:fold(fun({_, V}, Acc) -> V + Acc end, 7, T4),
    ?assertEqual(17, Res1).

even(X) when X >= 0 -> (X band 1) == 0.

filter_test(_) ->
    Dataset = get_dataset(),
    T4 = lists:nth(4, Dataset),
    T4_new = pre_dict:filter(fun({_, V}) -> even(V) end, T4),
    ?assertEqual(undefined, pre_dict:get("abc", T4_new)),
    ?assertEqual(2, pre_dict:get("acb", T4_new)),
    ?assertEqual(undefined, pre_dict:get("abcd", T4_new)),
    ?assertEqual(4, pre_dict:get("ab", T4_new)).

equal_test(_) ->
    Dataset = get_dataset(),
    T0 = pre_dict:new(),
    ?assertEqual(true, pre_dict:equal(get_new_pre_dict(), T0)),
    ?assertEqual(false, pre_dict:equal(lists:nth(1, Dataset), T0)),
    ?assertEqual(false, pre_dict:equal(lists:nth(2, Dataset), T0)),
    ?assertEqual(false, pre_dict:equal(lists:nth(3, Dataset), T0)),
    ?assertEqual(false, pre_dict:equal(lists:nth(4, Dataset), T0)),
    T1 = pre_dict:insert("abc", 1, T0),
    ?assertEqual(false, pre_dict:equal(get_new_pre_dict(), T1)),
    ?assertEqual(true, pre_dict:equal(lists:nth(1, Dataset), T1)),
    ?assertEqual(false, pre_dict:equal(lists:nth(2, Dataset), T1)),
    ?assertEqual(false, pre_dict:equal(lists:nth(3, Dataset), T1)),
    ?assertEqual(false, pre_dict:equal(lists:nth(4, Dataset), T1)),
    T2 = pre_dict:insert("acb", 2, T1),
    ?assertEqual(false, pre_dict:equal(get_new_pre_dict(), T2)),
    ?assertEqual(false, pre_dict:equal(lists:nth(1, Dataset), T2)),
    ?assertEqual(true, pre_dict:equal(lists:nth(2, Dataset), T2)),
    ?assertEqual(false, pre_dict:equal(lists:nth(3, Dataset), T2)),
    ?assertEqual(false, pre_dict:equal(lists:nth(4, Dataset), T2)),
    T3 = pre_dict:insert("abcd", 3, T2),
    ?assertEqual(false, pre_dict:equal(get_new_pre_dict(), T3)),
    ?assertEqual(false, pre_dict:equal(lists:nth(1, Dataset), T3)),
    ?assertEqual(false, pre_dict:equal(lists:nth(2, Dataset), T3)),
    ?assertEqual(true, pre_dict:equal(lists:nth(3, Dataset), T3)),
    ?assertEqual(false, pre_dict:equal(lists:nth(4, Dataset), T3)),
    T4 = pre_dict:insert("ab", 4, T3),
    ?assertEqual(false, pre_dict:equal(get_new_pre_dict(), T4)),
    ?assertEqual(false, pre_dict:equal(lists:nth(1, Dataset), T4)),
    ?assertEqual(false, pre_dict:equal(lists:nth(2, Dataset), T4)),
    ?assertEqual(false, pre_dict:equal(lists:nth(3, Dataset), T4)),
    ?assertEqual(true, pre_dict:equal(lists:nth(4, Dataset), T4)).

merge_test(_) ->
    A0 = pre_dict:new(),
    A1 = pre_dict:insert("a", 1, A0),
    A2 = pre_dict:insert("ba", 2, A1),
    A3 = pre_dict:insert("bb", 3, A2),

    B0 = pre_dict:new(),
    B1 = pre_dict:insert("c", 4, B0),
    B2 = pre_dict:insert("bc", 5, B1),

    C0 = pre_dict:merge(A3, B2),

    T0 = pre_dict:new(),
    T1 = pre_dict:insert("a", 1, T0),
    T2 = pre_dict:insert("ba", 2, T1),
    T3 = pre_dict:insert("bb", 3, T2),
    T4 = pre_dict:insert("c", 4, T3),
    T5 = pre_dict:insert("bc", 5, T4),

    ?assertEqual(true, pre_dict:equal(C0, T5)).
