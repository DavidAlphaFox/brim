%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim).

-export([add_class/2,
         add_class/3,
         attr/3,
         attr/4,
         class/2,
         class/3,
         content/2,
         content/3,
         do/2,
         document/1,
         id/2,
         id/3,
         map/4,
         print/1,
         render/1,
         remove_class/2,
         remove_class/3,
         snippet/2]).

render(Tree) ->
    brim_html:render(Tree).

print(Tree) ->
    io:format("~s~n", [brim_html:render(Tree)]).

document(Filename) ->
    brim_html:parse_file(Filename).

snippet(Tree, Selector) ->
    Predicate = brim_selectors:parse(Selector),
    ziptree:subtree(ziptree:find(Tree, Predicate)).

do(Tree, [Action|Sequence]) ->
    do(Action(Tree), Sequence);
do(Tree, []) ->
    Tree.

map(Tree, Selector, List, Transform) ->
    Snippet = snippet(Tree, Selector),
    lists:foldl(fun(E, Subtree) ->
        ziptree:insert_tree(Transform(Snippet, E), Subtree)
    end, ziptree:new(), List).

content(Selector, Content) ->
    fun(Tree) -> content(Tree, Selector, Content) end.

content(Tree, Selector, Content) ->
    Transform = content_transform(Content),
    ziptree:transform(Tree, predicate_transform(Selector, Transform)).

content_transform(Text) when is_binary(Text) ->
    Node = brim_html:text_node(Text),
    fun(Tree) ->
        ziptree:insert_child(Node, ziptree:delete_children(Tree))
    end;
content_transform(Text) when is_list(Text) ->
    content_transform(unicode:characters_to_binary(Text));
content_transform(Text) when is_atom(Text) ->
    content_transform(atom_to_binary(Text, unicode));
content_transform(Text) when is_integer(Text) ->
    content_transform(list_to_binary(integer_to_list(Text)));
content_transform(Subtree) ->
    case ziptree:is_ziptree(Subtree) of
        true ->
            fun(Tree) ->
                ziptree:replace_children_with_tree(Subtree, Tree)
            end;
        false ->
            erlang:error(badarg, [Subtree])
    end.

attr(Selector, Key, Value) when is_list(Key), is_list(Value) ->
    fun(Tree) -> attr(Tree, Selector, Key, Value) end.

attr(Tree, Selector, Key, Value) when is_list(Key), is_list(Value) ->
    ziptree:transform(Tree, predicate_transform(Selector, fun(Z) ->
        brim_html:set_attr(Z, Key, Value)
    end)).

id(Selector, ID) when is_list(ID) ->
    fun (Tree) -> id(Tree, Selector, ID) end.

id(Tree, Selector, ID) when is_list(ID) ->
    ziptree:transform(Tree, predicate_transform(Selector, fun(Z) ->
        brim_html:set_id(Z, ID)
    end)).

class(Selector, Class) when is_list(Class) ->
    fun (Tree) -> class(Tree, Selector, Class) end.

class(Tree, Selector, Class) when is_list(Class) ->
    ziptree:transform(Tree, predicate_transform(Selector, fun(Z) ->
        brim_html:set_class(Z, Class)
    end)).

add_class(Selector, Class) when is_list(Class) ->
    fun(Tree) -> add_class(Tree, Selector, Class) end.

add_class(Tree, Selector, Class) when is_list(Class) ->
    ziptree:transform(Tree, predicate_transform(Selector, fun(Z) ->
        brim_html:add_class(Z, Class)
    end)).

remove_class(Selector, Class) when is_list(Class) ->
    fun(Tree) -> remove_class(Tree, Selector, Class) end.

remove_class(Tree, Selector, Class) when is_list(Class) ->
    ziptree:transform(Tree, predicate_transform(Selector, fun(Z) ->
        brim_html:remove_class(Z, Class)
    end)).

predicate_transform(Selector, Transform) ->
    Predicate = brim_selectors:parse(Selector),
    fun(Tree) ->
        case Predicate(Tree) of
            true -> Transform(Tree);
            _    -> Tree
        end
    end.
