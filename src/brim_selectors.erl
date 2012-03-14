%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim_selectors).

-export([parse/1]).

-define(true, fun(_) -> true end).

parse(L) ->
    pred(brim_selector_lex:scan(L)).

pred(Selectors) ->
    pred(lists:reverse(Selectors), ?true).

pred([Selector|T], Pred) ->
    case Selector of
        {id, ID} ->
            pred(T, fun(Tree) ->
                brim_html:has_id(Tree, ID) andalso Pred(Tree)
            end);
        {class, Class} ->
            pred(T, fun(Tree) ->
                brim_html:has_class(Tree, Class) andalso Pred(Tree)
            end);
        {attrib, Attr} ->
            pred(T, fun(Tree) ->
                Pred(Tree) andalso brim_html:attribute(Tree, Attr) /= error
            end);
        {attrib, Attr, Test, Val} ->
            pred(T, fun(Tree) ->
                Pred(Tree) andalso
                    compare_attr(brim_html:attribute(Tree, Attr), Test, Val)
            end);
        {pseudo, "nth-child", N} ->
            pred(T, is_nth_pred(Pred, fun is_nth/3, fun ziptree:left/1, N));
        {pseudo, "nth-last-child", N} ->
            pred(T, is_nth_pred(Pred, fun is_nth/3, fun ziptree:right/1, N));
        {pseudo, "first-child"} ->
            pred(T, is_nth_pred(Pred, fun is_nth/3, fun ziptree:left/1, 1));
        {pseudo, "last-child"} ->
            pred(T, is_nth_pred(Pred, fun is_nth/3, fun ziptree:right/1, 1));
        {pseudo, "nth-of-type", N} ->
            pred(T, is_nth_pred(Pred, fun is_nth_of_type/3, fun ziptree:left/1, N));
        {pseudo, "nth-last-of-type", N} ->
            pred(T, is_nth_pred(Pred, fun is_nth_of_type/3, fun ziptree:right/1, N));
        {pseudo, "first-of-type"} ->
            pred(T, is_nth_pred(Pred, fun is_nth_of_type/3, fun ziptree:left/1, 1));
        {pseudo, "last-of-type"} ->
            pred(T, is_nth_pred(Pred, fun is_nth_of_type/3, fun ziptree:right/1, 1));
        {'not', Selectors} ->
            NotPred = pred(Selectors, ?true),
            pred(T, fun(Tree) ->
                Pred(Tree)
                    andalso brim_html:is_type(Tree, "*")
                    andalso not NotPred(Tree)
            end);
        {relation, parent} ->
            RelPred = pred(T, ?true),
            fun(Tree) ->
                Ancestor = ziptree:up(Tree),
                Ancestor /= error andalso RelPred(Ancestor) andalso Pred(Tree)
            end;
        {relation, ancestor} ->
            RelPred = pred(T, ?true),
            fun(Tree) ->
                Pred(Tree) andalso find_ancestors(Tree, RelPred)
            end;
        {element, Type} ->
            pred(T, fun(Tree) ->
                brim_html:is_type(Tree, Type) andalso Pred(Tree)
            end)
    end;
pred([], Pred) ->
    Pred.

compare_attr(error, _, _)       -> false;
compare_attr(A, equals,      B) -> A == B;
compare_attr(A, begins_with, B) -> lists:prefix(B, A);
compare_attr(A, ends_with,   B) -> lists:suffix(B, A);
compare_attr(A, contains,    B) -> string:str(A, B) > 0;
compare_attr(A, includes,    B) -> lists:member(B, string:tokens(A, " ")).

find_ancestors(Tree, Pred) ->
    case ziptree:up(Tree) of
        error  -> false;
        Parent -> Pred(Parent) orelse find_ancestors(Parent, Pred)
    end.

is_nth_pred(Pred, TestFun, StepFun, N) ->
    fun(Tree) ->
        Pred(Tree) andalso TestFun(Tree, StepFun, N)
    end.

is_nth(Tree, StepFun, N) ->
    is_nth_of_type(Tree, StepFun, "*", N).

is_nth_of_type(Tree, StepFun, N) ->
    is_nth_of_type(Tree, StepFun, brim_html:type(Tree), N).

is_nth_of_type(error, _, _, N) ->
    N =:= 0;
is_nth_of_type(Tree, StepFun, Type, N) when N >= 0 ->
    case brim_html:is_type(Tree, Type) of
        true -> is_nth_of_type(StepFun(Tree), StepFun, Type, N - 1);
        _    -> is_nth_of_type(StepFun(Tree), StepFun, Type, N)
    end;
is_nth_of_type(_, _, _, _) ->
    false.
