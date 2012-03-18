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
                {brim, FA} when FA == {content, 3};
                                FA == {map, 4};
                                FA == {attr, 4};
                                FA == {id, 3};
                                FA == {class, 3};
                                FA == {add_class, 3};
                                FA == {remove_class, 3} ->
                    maybe_replace(Tree);
                _ ->
                    Tree
            end;
        _ ->
            Tree
    end.

maybe_replace(Tree) ->
    [Arg1, Selector|Args] = erl_syntax:application_arguments(Tree),
    case erl_syntax:type(Selector) of
        string ->
            try brim_selector_lex:scan(erl_syntax:concrete(Selector)) of
                Parsed ->
                    ParsedTree = erl_syntax:abstract(Parsed),
                    Op = erl_syntax:application_operator(Tree),
                    erl_syntax:application(Op, [Arg1, ParsedTree|Args])
            catch
                _:Error ->
                    throw({Error, erl_syntax:get_pos(Tree)})
            end;
        _ ->
            Tree
    end.

filename(Forms) ->
    [{F, _}|_] = [ erl_syntax_lib:analyze_file_attribute(A)
                   || A <- Forms,
                      erl_syntax:type(A) == attribute,
                      file == element(1, erl_syntax_lib:analyze_attribute(A)) ],
    F.

format_error(Error) ->
    io_lib:format("Precompiling selector failed with ~p", [Error]).
