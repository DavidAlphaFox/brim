%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim_selector_lex).

-export([scan/1]).

-define(is_ident(C), C >= $a andalso C =< $z
              orelse C >= $A andalso C =< $Z
              orelse C >= $0 andalso C =< $9
              orelse C =:= $-
              orelse C =:= $_).

-define(is_ws(C), C =:= 32
           orelse C =:= 9).

-define(is_token(C), C =:= $#
              orelse C =:= $.
              orelse C =:= $:
              orelse C =:= $(
              orelse C =:= $)
              orelse C =:= $[
              orelse C =:= $]).

scan(String) ->
    try {selector, lex2(lex1(tokenize(string:strip(String))))}
    catch
        throw:{Error, Args} ->
            erlang:error(Error, Args)
    end.

lex1([ws, {match, _} = H|T])    -> lex1([H|T]);
lex1([ws, {relation, _} = H|T]) -> lex1([H|T]);
lex1([{match, _} = H, ws|T])    -> [H|lex1(T)];
lex1([{relation, _} = H, ws|T]) -> [H|lex1(T)];
lex1(['[', ws|T])               -> ['['|lex1(T)];
lex1([ws, ']'|T])               -> [']'|lex1(T)];
lex1(['(', ws|T])               -> ['('|lex1(T)];
lex1([ws, ')'|T])               -> [')'|lex1(T)];
lex1([ws|T])                    -> [{relation, ancestor}|lex1(T)];
lex1([H|T])                     -> [H|lex1(T)];
lex1([])                        -> [].

lex2(['#', {ident, I}|T]) ->
    [{id, I}|lex2(T)];
lex2(['.', {ident, I}|T]) ->
    [{class, I}|lex2(T)];
lex2([':', {ident, "not"}, '('|T]) ->
    {T1, T2} = match_parenthesis(T),
    [{pseudo, 'not', lex2(T1)}|lex2(T2)];
lex2([':', {ident, I}, '(', {ident, N}, ')'|T]) ->
    [{pseudo, list_to_atom(I), try_to_integer(N)}|lex2(T)];
lex2([':', {ident, I}|T]) ->
    [{pseudo, list_to_atom(I)}|lex2(T)];
lex2(['[', {ident, I}, ']'|T]) ->
    [{attrib, I}|lex2(T)];
lex2(['[', {ident, I}, {match, M}, {string, S}, ']'|T]) ->
    [{attrib, I, M, S}|lex2(T)];
lex2(['[', {ident, I}, {match, M}, {ident, S}, ']'|T]) ->
    [{attrib, I, M, S}|lex2(T)];
lex2([{ident, I}|T]) ->
    [{element, I}|lex2(T)];
lex2([{element, _} = H|T]) ->
    [H|lex2(T)];
lex2([{relation, _} = H|T]) ->
    [H|lex2(T)];
lex2([]) ->
    [];
lex2(Tokens) ->
    throw({syntax_error, [Tokens]}).

tokenize([]) ->
    [];
tokenize(S) ->
    {Token, R} = case S of
        [H|_] when ?is_ident(H) -> ident(S);
        [H|T] when ?is_token(H) -> {list_to_atom([H]), T};
        "="  ++ T               -> {{match, equals}, T};
        "~=" ++ T               -> {{match, includes}, T};
        "^=" ++ T               -> {{match, begins_with}, T};
        "$=" ++ T               -> {{match, ends_with}, T};
        "*=" ++ T               -> {{match, contains}, T};
        ">"  ++ T               -> {{relation, parent}, T};
        "+"  ++ T               -> {{relation, adjacent}, T};
        "~"  ++ T               -> {{relation, sibling}, T};
        "*"  ++ T               -> {{element, "*"}, T};
        [$"|T]                  -> string(T);
        [H|T] when ?is_ws(H)    -> whitespace(T);
        _                       -> throw({syntax_error, [S]})
    end,
    [Token|tokenize(R)].

ident(S) ->
    ident(S, []).

ident([H|T], A) when ?is_ident(H) ->
    ident(T, [H|A]);
ident(T, A) ->
    {{ident, lists:reverse(A)}, T}.

whitespace([H|T]) when ?is_ws(H) ->
    whitespace(T);
whitespace(T) ->
    {ws, T}.

string(S) ->
    string(S, []).

string([$"|T], A) ->
    {{string, lists:reverse(A)}, T};
string([H|T], A) ->
    string(T, [H|A]);
string([], A) ->
    throw({unterminated_string, [lists:reverse(A)]}).

match_parenthesis(L) ->
    match_parenthesis(L, [], 0).

match_parenthesis(['('|T], A, N) ->
    match_parenthesis(T, ['('|A], N + 1);
match_parenthesis([')'|T], A, 0) ->
    {lists:reverse(A), T};
match_parenthesis([')'|T], A, N) ->
    match_parenthesis(T, [')'|A], N - 1);
match_parenthesis([H|T], A, N) ->
    match_parenthesis(T, [H|A], N);
match_parenthesis([], A, _) ->
    throw({unmatched_parenthesis, [lists:reverse(A)]}).

try_to_integer(S) ->
    try list_to_integer(S)
    catch
        error:badarg -> S
    end.
