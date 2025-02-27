-module(pre_dict).

-export([new/0, add_element/2, remove_element/2, get_element/2,
  is_set/1, filter/2, from_list/1, to_list/1, map/2, foldl/3, foldr/3, merge/2]).

-include("pre_dict.hrl").

-opaque set(Value) ::
#set{storage :: array:array(Value), length :: integer()}.

-export_type([set/1]).

new() ->
  #set{storage = array:new(?INIT_CAPACITY), length = 0}.

is_set(#set{}) ->
  true;

is_set(_) ->
  false.

from_list(List) ->
  lists:foldl(fun add_element/2, new(), List).

to_list(#set{storage = Array, length = _}) ->
  to_list(Array, 0, array:size(Array), []).

to_list(Array, Index, Size, Acc) when Index < Size ->
  case array:get(Index, Array) of
    undefined ->
      to_list(Array, Index + 1, Size, Acc);
    Value ->
      to_list(Array, Index + 1, Size, [Value | Acc])
  end;

to_list(_, _, _, Acc) ->
  Acc.


add_element(Value, #set{storage = Array, length = Length}) ->
  case get_element(Value, #set{storage = Array, length = Length}) of
    found ->
      #set{storage = Array, length = Length}; % элемент уже существует, не добавляем
    not_found ->
      ResizedArray =
        case Length / array:size(Array) > ?LOAD_FACTOR of
          true ->
            grow_array(Array);
          false ->
            Array
        end,
      case put_element(Value, ResizedArray) of
        {changed_value, ReturnedArray} ->
          #set{storage = ReturnedArray, length = Length};
        {new_value, ReturnedArray} ->
          #set{storage = ReturnedArray, length = Length + 1}
      end
  end.

remove_element(Value, #set{storage = Array, length = Length}) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      #set{storage = Array, length = Length};
    Value ->
      NewArray = array:set(Position, undefined, Array),
      #set{storage = NewArray, length = Length - 1}
  end.

get_element(Value, #set{storage = Array, length = _}) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      not_found;
    Value ->
      found;
    _ -> % collision
      find_element(Value, Array, Position)
  end.

find_element(Value, Array, Position) ->
  Size = array:size(Array),
  WrappedPosition = Position rem Size,
  case array:get(WrappedPosition, Array) of
    undefined ->
      not_found;
    Value ->
      found;
    _ ->
      find_element(Value, Array, WrappedPosition + 1)
  end.

filter(Pred, #set{storage = Array, length = _}) ->
  filter(Pred, Array, 0, array:size(Array), new()).

filter(Pred, OldArray, Index, Size, NewSet) when Index < Size ->
  PredResult = Pred(array:get(Index, OldArray)),
  case array:get(Index, OldArray) of
    undefined ->
      filter(Pred, OldArray, Index + 1, Size, NewSet);
    Value when PredResult ->
      NewSet1 = add_element(Value, NewSet),
      filter(Pred, OldArray, Index + 1, Size, NewSet1);
    _ ->
      filter(Pred, OldArray, Index + 1, Size, NewSet)
  end;

filter(_, _, _, _, NewSet) ->
  NewSet.

map(Function, #set{storage = Array, length = _}) ->
  map(Function, Array, 0, array:size(Array), new()).

map(Function, OldArray, Index, Size, NewSet) when Index < Size ->
  case array:get(Index, OldArray) of
    undefined ->
      map(Function, OldArray, Index + 1, Size, NewSet);
    Value ->
      NewValue = Function(Value),
      NewSet1 = add_element(NewValue, NewSet),
      map(Function, OldArray, Index + 1, Size, NewSet1)
  end;

map(_, _, _, _, NewSet) ->
  NewSet.

foldl(Function, Acc, #set{storage = Array, length = _}) ->
  List = to_list(Array, 0, array:size(Array), []),
  lists:foldl(Function, Acc, List).

foldr(Function, Acc, #set{storage = Array, length = _}) ->
  List = to_list(Array, 0, array:size(Array), []),
  lists:foldr(Function, Acc, List).

merge(Set1, Set2) ->
  foldl(fun add_element/2, Set1, Set2).

put_element(Value, Array) ->
  Position = calc_hash(Value, Array),
  case array:get(Position, Array) of
    undefined ->
      {new_value, array:set(Position, Value, Array)};
    OldValue when OldValue == Value ->
      {changed_value, Array};
    OldValue when OldValue /= Value ->
      FreePosition = find_free_position(Position, Array),
      {new_value, array:set(FreePosition, Value, Array)}
  end.

find_free_position(Position, Array) ->
  Size = array:size(Array),
  WrappedPosition = Position rem Size,
  case array:get(WrappedPosition, Array) of
    undefined ->
      WrappedPosition;
    _ ->
      find_free_position(WrappedPosition + 1, Array)
  end.

grow_array(Array) ->
  NewSize = array:size(Array) * ?GROW_FACTOR,
  NewArray = array:new(NewSize),
  copy_array(Array, NewArray).

copy_array(Array, NewArray) ->
  Size = array:size(Array),
  copy_array(Array, NewArray, 0, Size).

copy_array(Array, NewArray, Index, Size) when Index < Size ->
  case array:get(Index, Array) of
    undefined ->
      copy_array(Array, NewArray, Index + 1, Size);
    Value ->
      {_, NewArray1} = put_element(Value, NewArray),
      copy_array(Array, NewArray1, Index + 1, Size)
  end;

copy_array(_, NewArray, _, _) ->
  NewArray.

calc_hash(Value, Array) ->
  erlang:phash2(Value, array:size(Array)).
