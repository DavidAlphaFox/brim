%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(ziplist).

-export([delete/1,
         from_list/1,
         insert_after/2,
         insert_before/2,
         insert_list/2,
         is_empty/1,
         is_ziplist/1,
         on_leftmost/1,
         on_rightmost/1,
         left/1,
         new/0,
         replace/2,
         rewind/1,
         right/1,
         to_list/1,
         value/1]).

new() ->
    {[], []}.

is_ziplist({Left, Right}) when is_list(Left), is_list(Right) ->
    true;
is_ziplist(_) ->
    false.

from_list(List) ->
    {[], List}.

to_list({Left, Right}) ->
    lists:reverse(Left, Right).

value({_, [Node|_]}) ->
    Node;
value({[], []}) ->
    error.

is_empty({[], []}) -> true;
is_empty({_,  _})  -> false.

on_leftmost({[], _}) -> true;
on_leftmost({_,  _}) -> false.

on_rightmost({[], []})  -> true;
on_rightmost({_,  [_]}) -> true;
on_rightmost({_,  _})   -> false.

left({[Node|Left], Right}) ->
    {Left, [Node|Right]};
left({[], _}) ->
    error.

right({Left, [Node|Right]}) when Right /= [] ->
    {[Node|Left], Right};
right({_, _}) ->
    error.

rewind({Left, Right}) ->
    {[], lists:reverse(Left, Right)}.

insert_before(Node, {Left, Right}) ->
    {Left, [Node|Right]}.

insert_after(Node, {[], []}) ->
    {[], [Node]};
insert_after(Node, {Left, [Current|Right]}) ->
    {[Current|Left], [Node|Right]}.

insert_list(Nodes, ZL) ->
    lists:foldl(fun insert_after/2, ZL, Nodes).

replace(Node, {Left, [_|Right]}) ->
    {Left, [Node|Right]}.

delete({[Last|Left], [_]}) ->
    {Left, [Last]};
delete({Left, [_|Right]}) ->
    {Left, Right};
delete({[_|Left], []}) ->
    {Left, []};
delete({[], []}) ->
    error.
