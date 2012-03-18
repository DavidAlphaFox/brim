%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim_parse_transform).

-export([parse_transform/2,
         format_error/1]).

parse_transform(Forms, _Options) ->
    try erl_syntax_lib:map(fun transform/1, erl_syntax:form_list(Forms)) of
        Tree ->
            erl_syntax:revert_forms(Tree)
    catch
        throw:{Error, Line} ->
            {error, [{filename(Forms), [{Line, ?MODULE, Error}]}], []}
    end.

transform(Tree) ->
    case erl_syntax:type(Tree) of
        application ->
            case erl_syntax_lib:analyze_application(Tree) of
                {sel, 1} ->
                    replacement_tree(Tree);
                _ ->
                    Tree
            end;
        _ ->
            Tree
    end.

replacement_tree(Tree) ->
    [Arg] = erl_syntax:application_arguments(Tree),
    try brim_selector_lex:scan(erl_syntax:concrete(Arg)) of
        Selector ->
            erl_syntax:abstract(Selector)
    catch
        _:Error ->
            throw({Error, erl_syntax:get_pos(Tree)})
    end.

filename(Forms) ->
    [{F, _}|_] = [ erl_syntax_lib:analyze_file_attribute(A)
                   || A <- Forms,
                      erl_syntax:type(A) == attribute,
                      file == element(1, erl_syntax_lib:analyze_attribute(A)) ],
    F.

format_error(Error) ->
    io_lib:format("Precompiling selector failed with ~p", [Error]).
