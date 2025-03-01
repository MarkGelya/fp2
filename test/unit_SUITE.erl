-module(unit_SUITE).

-include_lib("../src/pre_dict.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([grow_test/1, add_element_test/1, remove_test/1, filter_test/1, to_list_test/1,
  from_list_test/1, map_test/1, foldl_test/1, foldr_test/1,
  is_associative_test/1, has_identity_element_test/1, merge_test/1]).

all() ->
  [grow_test, add_element_test, remove_test, filter_test, to_list_test, from_list_test,
    map_test, foldl_test, foldr_test, is_associative_test, has_identity_element_test, merge_test].

% add few elements to test adding
add_element_test(_) ->
  Set = pre_dict:new(),
  Set1 = pre_dict:add_element(1, Set),
  Set2 = pre_dict:add_element(2, Set1),
  Set3 = pre_dict:add_element(3, Set2),
  ?assertEqual(3, Set3#set.length),
  ?assertEqual(found, pre_dict:get_element(1, Set3)),
  ?assertEqual(found, pre_dict:get_element(2, Set3)).

% add a lot of elements to test growing
grow_test(_) ->
  #set{storage = Array, length = Length} = add_elements(pre_dict:new(), 0),
  ?assertEqual(1000, Length),
  ?assert(array:size(Array) >= 1000).

% add 1000 elements and remove 3 of them to test removing
remove_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  Set1 = pre_dict:remove_element(1, Set),
  Set2 = pre_dict:remove_element(2, Set1),
  Set3 = pre_dict:remove_element(3, Set2),
  ?assertEqual(997, Set3#set.length),
  ?assertEqual(not_found, pre_dict:get_element(1, Set3)),
  ?assertEqual(not_found, pre_dict:get_element(2, Set3)),
  ?assertEqual(not_found, pre_dict:get_element(3, Set3)),
  ?assertEqual(found, pre_dict:get_element(4, Set3)).

add_elements(Set, Count) when Count < 1000 ->
  NewSet = pre_dict:add_element(Count, Set),
  add_elements(NewSet, Count + 1);
add_elements(Set, _) ->
  Set.

filter_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  FilteredSet = pre_dict:filter(fun(X) -> X > 100 end, Set),
  ?assertEqual(899, FilteredSet#set.length),
  ?assertEqual(not_found, pre_dict:get_element(1, FilteredSet)),
  ?assertEqual(not_found, pre_dict:get_element(52, FilteredSet)),
  ?assertEqual(not_found, pre_dict:get_element(100, FilteredSet)),
  ?assertEqual(found, pre_dict:get_element(101, FilteredSet)).

to_list_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  List = pre_dict:to_list(Set),
  ?assertEqual(1000, length(List)),
  ?assertEqual(lists:seq(0, 999), lists:sort(List)).

from_list_test(_) ->
  List = lists:seq(0, 999),
  Set = pre_dict:from_list(List),
  ?assertEqual(1000, Set#set.length),
  ?assertEqual(found, pre_dict:get_element(1, Set)),
  ?assertEqual(found, pre_dict:get_element(555, Set)),
  ?assertEqual(found, pre_dict:get_element(321, Set)).

map_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  NewSet = pre_dict:map(fun(X) -> X + 1 end, Set),
  NewSetMultiple = pre_dict:map(fun(X) -> X * 2 end, NewSet),
  ?assertEqual(1000, NewSet#set.length),
  ?assertEqual(1000, NewSetMultiple#set.length),
  ?assertEqual(lists:seq(1, 1000),
    lists:sort(pre_dict:to_list(NewSet))),
  ?assertEqual([X || X <- lists:seq(1, 2000), X rem 2 =:= 0],
    lists:sort(pre_dict:to_list(NewSetMultiple))).

foldl_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  MultSet = pre_dict:add_element(2, pre_dict:add_element(1, pre_dict:new())),
  Sum = pre_dict:foldl(fun(X, Acc) -> X + Acc end, 0, Set),
  Multiplied = pre_dict:foldl(fun(X, Acc) -> X * Acc end, 1, Set),
  Multiplied2 = pre_dict:foldl(fun(X, Acc) -> X * Acc end, 1, MultSet),
  ?assertEqual(499500, Sum),
  ?assertEqual(0, Multiplied),
  ?assertEqual(2, Multiplied2).

foldr_test(_) ->
  Set = add_elements(pre_dict:new(), 0),
  MultSet = pre_dict:add_element(2, pre_dict:add_element(1, pre_dict:new())),
  Sum = pre_dict:foldr(fun(X, Acc) -> X + Acc end, 0, Set),
  Multiplied = pre_dict:foldr(fun(X, Acc) -> X * Acc end, 1, Set),
  Multiplied2 = pre_dict:foldr(fun(X, Acc) -> X * Acc end, 1, MultSet),
  ?assertEqual(499500, Sum),
  ?assertEqual(0, Multiplied),
  ?assertEqual(2, Multiplied2).

is_associative_test(_) ->
  A = pre_dict:add_element(1, pre_dict:new()),
  B = pre_dict:add_element(2, pre_dict:new()),
  AB = pre_dict:add_element(2, A),
  ABC1 = pre_dict:add_element(3, AB),
  BC = pre_dict:add_element(3, B),
  ABC2 = pre_dict:add_element(1, BC),
  ?assertEqual(ABC2, ABC1).

has_identity_element_test(_) ->
  EmptySet = pre_dict:new(),
  A = pre_dict:add_element(1, EmptySet),
  ?assertEqual(A, pre_dict:add_element(1, pre_dict:new())).

merge_test(_) ->
  Set1 = pre_dict:from_list(lists:seq(1, 1000)),
  Set2 = pre_dict:from_list(lists:seq(1001, 2000)),
  Merged = pre_dict:merge(Set1, Set2),
  MergedWithSelf = pre_dict:merge(Set1, Set1),
  ?assertEqual(2000, Merged#set.length),
  ?assertEqual(found, pre_dict:get_element(1, Merged)),
  ?assertEqual(found, pre_dict:get_element(1000, Merged)),
  ?assertEqual(found, pre_dict:get_element(1001, Merged)),
%%  ?assertEqual(1000, MergedWithSelf#set.length),
  ?assertEqual(lists:seq(1, 1000), lists:sort(pre_dict:to_list(MergedWithSelf))).
