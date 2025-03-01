-module(default_property_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("../src/pre_dict.hrl").

-define(SET_SIZE, 1000).


-export([all/0, add_element_property_test/1, remove_element_property_test/1, size_property_test/1]).

all() ->
  [add_element_property_test, remove_element_property_test, size_property_test].


add_element_property_test(_) ->
  ?FORALL({Set, Element}, {set(), int()},
    begin
      NewSet = pre_dict:add_element(Element, Set),
      ?assertEqual(found, pre_dict:get_element(Element, NewSet))
    end).

remove_element_property_test(_) ->
  ?FORALL({Set, Element}, {set(), int()},
    begin
      SetWithElement = pre_dict:add_element(Element, Set),
      NewSet = pre_dict:remove_element(Element, SetWithElement),
      ?assertEqual(not_found, pre_dict:get_element(Element, NewSet))
    end).

size_property_test(_) ->
  ?FORALL(Elements, list(int()),
    begin
      Set = lists:foldl(fun(E, Acc) -> pre_dict:add_element(E, Acc) end,
        pre_dict:new(), Elements),
      ?assertEqual(length(lists:usort(Elements)), Set#set.length)
    end).
