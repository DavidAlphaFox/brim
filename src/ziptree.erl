%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(ziptree).

-export([delete/1,
         delete_children/1,
         down/1,
         find/2,
         insert/2,
         insert_branch/2,
         insert_child/2,
         insert_tree/2,
         is_ziptree/1,
         on_branch/1,
         on_nonempty_branch/1,
         left/1,
         new/0,
         replace/2,
         replace_children_with_tree/2,
         replace_value/2,
         rewind/1,
         right/1,
         root/1,
         siblings/1,
         subtree/1,
         transform/2,
         up/1,
         value/1]).

new() ->
    {[], ziplist:new()}.

is_ziptree({Path, Siblings}) when is_list(Path) ->
    ziplist:is_ziplist(Siblings);
is_ziptree(_) ->
    false.

value({_, Siblings}) ->
    case ziplist:value(Siblings) of
        {V, _} -> V;
        {V}    -> V;
        error  -> error
    end.

on_branch({_, Siblings}) ->
    case ziplist:value(Siblings) of
        {_, _}  -> true;
        _       -> false
    end.

on_nonempty_branch({_, Siblings}) ->
    case ziplist:value(Siblings) of
        {_, [_|_]} -> true;
        _          -> false
    end.

subtree({_, Siblings}) ->
    case ziplist:value(Siblings) of
        error -> error;
        Node  -> {[], ziplist:insert_after(Node, ziplist:new())}
    end.
 
left({Path, Siblings0}) ->
    case ziplist:left(Siblings0) of
        error     -> error;
        Siblings1 -> {Path, Siblings1}
    end.

right({Path, Siblings0}) ->
    case ziplist:right(Siblings0) of
        error     -> error;
        Siblings1 -> {Path, Siblings1}
    end.

rewind({Path, Siblings}) ->
    {Path, ziplist:rewind(Siblings)}.

down({Path, Siblings}) ->
    case ziplist:value(Siblings) of
        {Val, Children} ->
            {[ziplist:replace(Val, Siblings)|Path], ziplist:from_list(Children)};
        _ ->
            error
    end.

up({[Elders|Path], Siblings}) ->
    Node = {ziplist:value(Elders), ziplist:to_list(Siblings)},
    {Path, ziplist:replace(Node, Elders)};
up({[], _}) ->
    error.

root(Tree0) ->
    case up(Tree0) of
        error -> rewind(Tree0);
        Tree1 -> root(Tree1)
    end.

siblings({_, Siblings}) ->
    ziplist:to_list(Siblings).

insert(Val, {Path, Siblings}) ->
    {Path, ziplist:insert_after({Val}, Siblings)}.

insert_branch(Val, {Path, Siblings}) ->
    {Path, ziplist:insert_after({Val, []}, Siblings)}.

insert_tree(Subtree, {Path, Siblings}) ->
    {Path, ziplist:insert_list(siblings(root(Subtree)), Siblings)}.

insert_child(Child, {Path, Siblings}) ->
    {Val, Children} = ziplist:value(Siblings),
    {Path, ziplist:replace({Val, [{Child}|Children]}, Siblings)}.

replace(Val, {Path, Siblings}) ->
    {Path, ziplist:replace({Val}, Siblings)}.

replace_value(Val, {Path, Siblings}) ->
    case ziplist:value(Siblings) of
        {_, Children} ->
            {Path, ziplist:replace({Val, Children}, Siblings)};
        {_} ->
            {Path, ziplist:replace({Val}, Siblings)}
    end.

replace_children_with_tree(Subtree, Tree) ->
    up(insert_tree(Subtree, down(delete_children(Tree)))).

delete({Path, Siblings}) ->
    {Path, ziplist:delete(Siblings)}.

delete_children({Path, Siblings}) ->
    {Val, _} = ziplist:value(Siblings),
    {Path, ziplist:replace({Val, []}, Siblings)}.

transform(Tree, Transform) ->
    do_transform(root(Tree), Transform).

do_transform(Tree0, Transform) ->
    Tree1 = case on_nonempty_branch(Tree0) of
        true -> up(do_transform(down(Tree0), Transform));
        _    -> Tree0
    end,
    Tree2 = Transform(Tree1),
    case right(Tree2) of
        error -> Tree2;
        Tree3 -> do_transform(Tree3, Transform)
    end.

find(Tree, Predicate) ->
    do_find(root(Tree), Predicate).

do_find(error, _) ->
    error;
do_find(Tree0, Predicate) ->
    case Predicate(Tree0) of
        true ->
            Tree0;
        false ->
            case do_find(down(Tree0), Predicate) of
                error -> do_find(right(Tree0), Predicate);
                Tree1 -> Tree1
            end
    end.

