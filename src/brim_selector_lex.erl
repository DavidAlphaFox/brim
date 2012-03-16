%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim_selector_lex).

-export([scan/1]).

-define(is_ident(C), C >= $a andalso C =< $z
              orelse C >= $A andalso C =< $Z
              orelse C >= $0 andalso C =< $9
              orelse C =:= $-
              orelse C =:= $_).

-define(is_whitespace(C), C =:= 32
                   orelse C =:= 9).

-define(is_combinator(C), C =:= 32
                   orelse C =:= $>
                   orelse C =:= $+
                   orelse C =:= $-).

-define(is_match(C), C =:= $=
              orelse C =:= $~
              orelse C =:= $^
              orelse C =:= $$).  % *= is handled as a special case

scan(String) ->
    try lex(parse_ws(tokenize(String)))
    catch
        throw:{Error, Args} ->
            erlang:error(Error, Args)
    end.

lex(['#', {ident, I}|T]) ->
    [{id, I}|lex(T)];
lex(['.', {ident, I}|T]) ->
    [{class, I}|lex(T)];
lex([':', {ident, "not"}, '('|T]) ->
    {T1, T2} = match_parenthesis(T),
    [{'not', lex(T1)}|lex(T2)];
lex([':', {ident, I}, '(', {ident, N}, ')'|T]) ->
    [{pseudo, I, try_to_integer(N)}|lex(T)];
lex([':', {ident, I}|T]) ->
    [{pseudo, I}|lex(T)];
lex(['[', {ident, I}, ']'|T]) ->
    [{attrib, I}|lex(T)];
lex(['[', {ident, I}, {match, M}, {string, S}, ']'|T]) ->
    [{attrib, I, M, S}|lex(T)];
lex(['[', {ident, I}, {match, M}, {ident, S}, ']'|T]) ->
    [{attrib, I, M, S}|lex(T)];
lex([{ident, I}|T]) ->
    [{element, I}|lex(T)];
lex([{relation, _} = H|T]) ->
    [H|lex(T)];
lex([]) ->
    [];
lex(Tokens) ->
    throw({syntax_error, [Tokens]}).

parse_ws([ws, {match, _} = H|T])    -> parse_ws([H|T]);
parse_ws([ws, {relation, _} = H|T]) -> parse_ws([H|T]);
parse_ws([{match, _} = H, ws|T])    -> [H|parse_ws(T)];
parse_ws([{relation, _} = H, ws|T]) -> [H|parse_ws(T)];
parse_ws(['[', ws|T])               -> ['['|parse_ws(T)];
parse_ws([ws, ']'|T])               -> [']'|parse_ws(T)];
parse_ws(['(', ws|T])               -> ['('|parse_ws(T)];
parse_ws([ws, ')'|T])               -> [')'|parse_ws(T)];
parse_ws([ws|T])                    -> [{relation, ancestor}|parse_ws(T)];
parse_ws([H|T])                     -> [H|parse_ws(T)];
parse_ws([])                        -> [].

tokenize([]) ->
    [];
tokenize([H|T] = S) ->
    {Token, R} = if  ?is_whitespace(H) -> whitespace(S);
                     ?is_combinator(H) -> combinator(S);
                     H =:= $*          -> star(S);
                     ?is_match(H)      -> match(S);
                     ?is_ident(H)      -> ident(S);
                     H =:= $"          -> string(T);
                     H =:= $#          -> {'#', T};
                     H =:= $.          -> {'.', T};
                     H =:= $:          -> {':', T};
                     H =:= $(          -> {'(', T};
                     H =:= $)          -> {')', T};
                     H =:= $[          -> {'[', T};
                     H =:= $]          -> {']', T};
                     true              -> throw({syntax_error, [S]})
                 end,
    [Token|tokenize(R)].

star("*=" ++ _ = S) ->
    match(S);
star(S) ->
    ident(S).

ident([$*|T]) ->
    {{ident, "*"}, T};
ident(S) ->
    ident(S, []).

ident([H|T], A) when ?is_ident(H) ->
    ident(T, [H|A]);
ident(T, A) ->
    {{ident, lists:reverse(A)}, T}.

whitespace([H|T]) when ?is_whitespace(H) ->
    whitespace(T);
whitespace(T) ->
    {ws, T}.

match("="  ++ T) -> {{match, equals}, T};
match("~=" ++ T) -> {{match, includes}, T};
match("^=" ++ T) -> {{match, begins_with}, T};
match("$=" ++ T) -> {{match, ends_with}, T};
match("*=" ++ T) -> {{match, contains}, T};
match(T)         -> throw({syntax_error, [T]}).

combinator(S) ->
    combinator(S, []).

combinator([H|T], A) when ?is_combinator(H) ->
    combinator(T, [H|A]);
combinator(T, A) ->
    case string:strip(lists:reverse(A)) of
        ">" -> {{relation, parent}, T};
        "+" -> {{relation, adjacent}, T};
        "-" -> {{relation, sibling}, T};
        M   -> throw({syntax_error, [M]})
    end.

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
