# Лабораторная работа №2 по дисциплине "Функциональное Программирование"

## Геля Марк Романович, P3332, Вариант: `pre-dict` (`Dict`, `Prefix Tree`)

## Задание

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами
тестирования (unit testing, property-based testing).

## Требования:

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства
   моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют
   получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про
   экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Реализация

### Cтруктура
```erlang
-record(pre_dict_node, {children, value, exists}).
```

### get
```erlang
get([], Node) ->
    Node#pre_dict_node.value;
get([Key | T], Node) ->
    case maps:find(Key, Node#pre_dict_node.children) of
        {ok, SubNode} ->
            get(T, SubNode);
        error ->
            undefined
    end.
```

### insert
```erlang
insert(Key, Value, Node) ->
    insert_impl(Key, Value, Node, []).

insert_impl([], Value, Child, Acc) ->
    NewChild = Child#pre_dict_node{value = Value, exists = true},
    insert2_impl(Acc, NewChild);
insert_impl([Key | T], Value, Node, Acc) ->
    case maps:find(Key, Node#pre_dict_node.children) of
        {ok, Child} ->
            insert_impl(T, Value, Child, [{Key, Node} | Acc]);
        error ->
            {KeyNB, NB} = new_branch([Key | T], Value),
            insert2_impl([{KeyNB, Node} | Acc], NB)
    end.

insert2_impl([], Child) ->
    Child;
insert2_impl([{Key, Parent} | Parents], Child) ->
    Map = maps:put(Key, Child, Parent#pre_dict_node.children),
    NewParent =
        #pre_dict_node{children = Map,
                       value = Parent#pre_dict_node.value,
                       exists = Parent#pre_dict_node.exists},
    insert2_impl(Parents, NewParent).
```

### delete
```erlang
delete(Key, Node) ->
    delete_impl(Key, Node, []).

delete_impl([], TargetNode, Acc) ->
    case maps:size(TargetNode#pre_dict_node.children) of
        0 ->
            delete_impl_reduce(Acc);
        _ ->
            NewNode = TargetNode#pre_dict_node{value = undefined, exists = false},
            delete2_impl(NewNode, Acc)
    end;
delete_impl([ChildKey | T], Parent, Acc) ->
    case maps:find(ChildKey, Parent#pre_dict_node.children) of
        {ok, Child} ->
            delete_impl(T, Child, [{ChildKey, Parent} | Acc]);
        error ->
            not_found
    end.

delete_impl_reduce([]) ->
    new();
delete_impl_reduce([{ChildKey, Parent} | Acc]) ->
    case (maps:size(Parent#pre_dict_node.children) == 1) and not Parent#pre_dict_node.exists
    of
        true ->
            delete_impl_reduce(Acc);
        false ->
            NewChildren = maps:remove(ChildKey, Parent#pre_dict_node.children),
            NewNode = Parent#pre_dict_node{children = NewChildren},
            delete2_impl(NewNode, Acc)
    end.

delete2_impl(Child, []) ->
    Child;
delete2_impl(Child, [{Key, Parent} | Parents]) ->
    NewChildren = maps:put(Key, Child, Parent#pre_dict_node.children),
    NewNode = Parent#pre_dict_node{children = NewChildren},
    delete2_impl(NewNode, Parents).
```

### to_list
```erlang
to_list(Node) ->
    to_list_impl(Node, [], []).

to_list_impl(#pre_dict_node{children = Children,
                            value = Value,
                            exists = true},
             AccPath,
             Acc) ->
    NewAcc =
        if Value =/= undefined ->
               [{lists:reverse(AccPath), Value} | Acc];
           true ->
               Acc
        end,
    lists:foldl(fun({K, V}, AccIn) -> to_list_impl(V, [K | AccPath], AccIn) end,
                NewAcc,
                maps:to_list(Children));
to_list_impl(#pre_dict_node{children = Children}, AccPath, Acc) ->
    lists:foldl(fun({K, V}, AccIn) -> to_list_impl(V, [K | AccPath], AccIn) end,
                Acc,
                maps:to_list(Children)).
```

### map, filter, fold
```erlang
map(Fun, Node) ->
    List = to_list(Node),
    NewPairs = lists:map(fun({K, V}) -> {K, Fun({K, V})} end, List),
    build_tree(NewPairs).

filter(Fun, Node) ->
    List = to_list(Node),
    NewPairs = lists:filter(Fun, List),
    build_tree(NewPairs).

fold(Fun, Acc, Node) ->
    List = to_list(Node),
    lists:foldl(fun({K, V}, A) -> Fun({K, V}, A) end, Acc, List).
```

### equal
```erlang
equal(Node1, Node2) ->
    case {Node1, Node2} of
        {#pre_dict_node{children = Children1,
                        value = Value1,
                        exists = Exists1},
         #pre_dict_node{children = Children2,
                        value = Value2,
                        exists = Exists2}} ->
            (Value1 =:= Value2)
            and (Exists1 =:= Exists2)
            and (maps:size(Children1) =:= maps:size(Children2))
            and lists:all(fun({Key, Child1}) ->
                             case maps:find(Key, Children2) of
                                 {ok, Child2} ->
                                     equal(Child1, Child2);
                                 error ->
                                     false
                             end
                          end,
                          maps:to_list(Children1));
        _ ->
            false
    end.
```

### merge
```erlang
merge(Node1, Node2) ->
    case Node2 of
        #pre_dict_node{children = Children2,
                       value = Value2,
                       exists = Exists2} ->
            MergedNode = merge_values(Node1, Value2, Exists2),
            NewChildren =
                lists:foldl(fun({Key, Child2}, Acc) ->
                               case maps:find(Key, MergedNode#pre_dict_node.children) of
                                   {ok, Child1} ->
                                       NewChild = merge(Child1, Child2),
                                       maps:put(Key, NewChild, Acc);
                                   error ->
                                       maps:put(Key, Child2, Acc)
                               end
                            end,
                            MergedNode#pre_dict_node.children,
                            maps:to_list(Children2)),
            MergedNode#pre_dict_node{children = NewChildren}
    end.

merge_values(Node, Value2, Exists2) ->
    case Node of
        #pre_dict_node{value = Value1, exists = Exists1} ->
            NewExists = Exists1 or Exists2,
            NewValue =
                case {Exists1, Exists2} of
                    {true, true} ->
                        Value1;
                    {true, false} ->
                        Value1;
                    {false, true} ->
                        Value2;
                    {false, false} ->
                        undefined
                end,
            Node#pre_dict_node{value = NewValue, exists = NewExists}
    end.
```

## Тестирование

В рамках тестирования были протестированы все функции из интерфейса, а так же были протестированы свойства моноида.

#  Вывод
При реализации ```fold```, ```map```, ```filter``` для дерева возникла проблема, связанная с рекурсивным обходом. Стандартный рекурсивный обход дерева, углубляясь по ветвям, увеличивает стек вызовов, что приводит к потенциальным проблемам с производительностью (замедление работы) и чрезмерным потреблением памяти, особенно для глубоких или больших деревьев.

Для решения этой проблемы был рассмотрен вариант использования хвостовой рекурсии с аккумулятором, так как данный подход применим для всех трех функций, то формирования списка была создана функция ```to_list```.
