%%% Copyright (c) 2012 Per Melin <p@greendale.se>. See file LICENSE.

-module(brim_html).

-export([parse/1,
         parse_file/1,
         render/1]).

-export([type/1,
         id/1,
         classes/1,
         attribute/2,
         is_type/2,
         is_element/1,
         is_empty/1,
         has_id/2,
         has_class/2]).

-export([set_attr/3,
         set_id/2,
         set_class/2,
         add_class/2,
         remove_class/2]).

-export([text_node/1]).

-define(is_whitespace(C), C =:= 32
                   orelse C =:= 10
                   orelse C =:= 13
                   orelse C =:= 9).

parse(Bytes) ->
    trane:sax(Bytes, fun zip_token/2, ziptree:new()).

parse_file(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    parse(Bytes).

type(Tree) ->
    case ziptree:value(Tree) of
        {tag, Type, _, _, _} -> Type;
        _                    -> error
    end.

id(Tree) ->
    case ziptree:value(Tree) of
        {tag, _, ID, _, _} -> ID;
        _                  -> error
    end.

classes(Tree) ->
    case ziptree:value(Tree) of
        {tag, _, _, Classes, _} -> Classes;
        _                       -> error
    end.

attribute(Tree, "id") ->
    id(Tree);
attribute(Tree, "class") ->
    string:join(classes(Tree), " ");
attribute(Tree, Attr) ->
    {tag, _, _, _, Attrs} = ziptree:value(Tree),
    value(Attr, Attrs).

is_type(Tree, "*") ->
    is_element(Tree);
is_type(Tree, Type) ->
    type(Tree) == Type.

has_id(Tree, ID) ->
    id(Tree) == ID.

has_class(Tree, ID) ->
    case classes(Tree) of
        error   -> false;
        Classes -> lists:member(ID, Classes)
    end.

is_element(Tree) ->
    type(Tree) /= error.

is_empty(Tree) ->
    is_element(Tree) andalso not ziptree:on_nonempty_branch(Tree).

set_attr(Tree, "id", ID) ->
    set_id(Tree, ID);
set_attr(Tree, "class", Class) ->
    set_class(Tree, Class);
set_attr(Tree, Key, Value) ->
    {tag, Tag, ID, Class, Attr0} = ziptree:value(Tree),
    Attr1 = lists:keystore(Key, 1, Attr0, {Key, Value}),
    ziptree:replace_value({tag, Tag, ID, Class, Attr1}, Tree).

set_id(Tree, ID) ->
    {tag, Tag, _, Class, Attr0} = ziptree:value(Tree),
    ziptree:replace_value({tag, Tag, ID, Class, Attr0}, Tree).

set_class(Tree, Class) ->
    {tag, Tag, ID, _, Attr0} = ziptree:value(Tree),
    ziptree:replace_value({tag, Tag, ID, [Class], Attr0}, Tree).

add_class(Tree, Class) ->
    {tag, Tag, ID, Classes, Attr0} = ziptree:value(Tree),
    case lists:member(Class, Classes) of
        true ->
            Tree;
        false ->
            ziptree:replace_value({tag, Tag, ID, [Class|Classes], Attr0}, Tree)
    end.

remove_class(Tree, Class) ->
    {tag, Tag, ID, Classes, Attr0} = ziptree:value(Tree),
    ziptree:replace_value({tag, Tag, ID, Classes -- [Class], Attr0}, Tree).

text_node(String) ->
    {text, String}.

zip_token({tag, Tag, Attr0}, Tree) ->
    {ID, Attr1} = keytake("id", Attr0),
    {Class, Attr2} = keytake("class", Attr1),
    Node = {tag, Tag, ID, string:tokens(Class, " "), Attr2},
    case is_void_element(Tag) of
        true ->
            ziptree:insert(Node, Tree);
        false ->
            ziptree:down(ziptree:insert_branch(Node, Tree))
    end;
zip_token({end_tag, Tag}, Tree) ->
    case is_void_element(Tag) of
        true  -> Tree;
        false -> ziptree:up(Tree)
    end;
zip_token({text, Text}, Tree) ->
    ziptree:insert({text, trim(Text)}, Tree);
zip_token(E, Tree) ->
    ziptree:insert(E, Tree).

render(Tree) ->
    do_render(ziptree:root(Tree)).

do_render(error) ->
    [];
do_render(Tree) ->
    case ziptree:value(Tree) of
        {tag, Tag, ID, Class, Attr} ->
            case is_void_element(Tag) of
                true ->
                    [$<, Tag, render_attr(ID, Class, Attr), $>
                     |do_render(ziptree:right(Tree))];
                false ->
                    [$<, Tag, render_attr(ID, Class, Attr), $>,
                     do_render(ziptree:down(Tree)),
                     $<, $/, Tag, $>|do_render(ziptree:right(Tree))]
            end;
        {text, Text} ->
            [Text|do_render(ziptree:right(Tree))];
        {comment, Text} ->
            ["<!--", Text, "-->"|do_render(ziptree:right(Tree))];
        {'!', Text} ->
            [$<, $!, Text, $>|do_render(ziptree:right(Tree))];
        {'?', Text} ->
            [$<, $?, Text, $?, $>|do_render(ziptree:right(Tree))];
        error ->
            []
    end.

render_attr(ID, Class, Attr) ->
    case Class of
        [] -> render_attr(ID, Attr);
        _  -> render_attr(ID, [{"class", string:join(Class, " ")}|Attr])
    end.

render_attr(ID, Attr) ->
    case ID of
        [] -> render_attr(Attr);
        _  -> render_attr([{"id", ID}|Attr])
    end.

render_attr([{K, V}|T]) ->
    [32, K, $=, $", V, $"|render_attr(T)];
render_attr([]) ->
    "".

trim(Bin) ->
    trim_tail(trim_head(Bin)).

trim_head(Bin) ->
    case do_trim_head(Bin) of
        Bin  -> Bin;
        Trim -> <<32, Trim/binary>>
    end.

do_trim_head(<<C, T/binary>>) when ?is_whitespace(C) ->
    do_trim_head(T);
do_trim_head(Bin) ->
    Bin.

trim_tail(Bin) ->
    case trim_tail(Bin, byte_size(Bin) - 1) of
        Bin  -> Bin;
        Trim -> <<Trim/binary, 32>>
    end.

trim_tail(Bin, Pos) when Pos > 0 ->
    case Bin of
        <<H:Pos/binary, C>> when ?is_whitespace(C) ->
            trim_tail(H, Pos - 1);
        _ ->
            Bin
    end;
trim_tail(Bin, _) ->
    Bin.

keytake(Key, List0) ->
    case lists:keytake(Key, 1, List0) of
        {value, {_, Val}, List1} ->
            {Val, List1};
        false ->
            {[], List0}
    end.

%% Faster than proplists:get_value/2.
%%
value(K, [{K,V}|_]) -> V;
value(K, [_|T])     -> value(K, T);
value(_, [])        -> error.

%% http://dev.w3.org/html5/spec-author-view/syntax.html#void-elements
%%
is_void_element("area")    -> true;
is_void_element("base")    -> true;
is_void_element("br")      -> true;
is_void_element("col")     -> true;
is_void_element("command") -> true;
is_void_element("embed")   -> true;
is_void_element("hr")      -> true;
is_void_element("img")     -> true;
is_void_element("input")   -> true;
is_void_element("keygen")  -> true;
is_void_element("link")    -> true;
is_void_element("meta")    -> true;
is_void_element("param")   -> true;
is_void_element("source")  -> true;
is_void_element("track")   -> true;
is_void_element("wbr")     -> true;
is_void_element(_)         -> false.
