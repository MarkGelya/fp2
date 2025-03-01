-module(monoid_property_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("../src/pre_dict.hrl").

-define(SET_SIZE, 1000).

-export([all/0, identity_property_test/1, associativity_property_test/1,
  idempotence_property_test/1]).

all() ->
  [identity_property_test, associativity_property_test, idempotence_property_test].

identity_property_test(_) ->
  ?FORALL(Set, set(),
    begin
      EmptySet = pre_dict:new(),
      ?assertEqual(Set, pre_dict:merge(Set, EmptySet)),
      ?assertEqual(Set, pre_dict:merge(EmptySet, Set))
    end).

associativity_property_test(_) ->
  ?FORALL({Set1, Set2, Set3}, {set(), set(), set()},
    begin
      Merged1 = pre_dict:merge(Set1, pre_dict:merge(Set2, Set3)),
      Merged2 = pre_dict:merge(pre_dict:merge(Set1, Set2), Set3),
      ?assertEqual(Merged1, Merged2)
    end).

idempotence_property_test(_) ->
  ?FORALL(Set, set(),
    begin
      Merged = pre_dict:merge(Set, Set),
      ?assertEqual(Set, Merged)
    end).

set() ->
  ?LET(Elements, list(int()),
    lists:foldl(fun(E, Acc) -> pre_dict:add_element(E, Acc) end, pre_dict:new(), Elements)).
