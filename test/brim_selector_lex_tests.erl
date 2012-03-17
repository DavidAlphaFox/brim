-module(brim_selector_lex_tests).

-import(brim_selector_lex, [scan/1]).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual([{element, "*"}],          scan("*")),
    ?assertEqual([{element, "div"}],        scan("div")),
    ?assertEqual([{class, "foo"}],          scan(".foo")),
    ?assertEqual([{id, "foo"}],             scan("#foo")),
    ?assertEqual([{pseudo, 'first-child'}], scan(":first-child")).

attrib_test() ->
    ?assertEqual([{attrib, "src"}],                     scan("[src]")),
    ?assertEqual([{attrib, "src", equals, "foo"}],      scan("[src=foo]")),
    ?assertEqual([{attrib, "src", begins_with, "foo"}], scan("[src^=foo]")),
    ?assertEqual([{attrib, "src", ends_with, "foo"}],   scan("[src$=foo]")),
    ?assertEqual([{attrib, "src", contains, "foo"}],    scan("[src*=foo]")),
    ?assertEqual([{attrib, "src", includes, "foo"}],    scan("[src~=foo]")),
    ?assertEqual([{attrib, "src", equals, "f b"}],      scan("[src=\"f b\"]")).

attrib_whitespace_test() ->
    ?assertEqual([{attrib, "src", equals, "foo"}],      scan("[ src = foo ]")),
    ?assertEqual([{attrib, "src", begins_with, "foo"}], scan("[ src ^= foo ]")),
    ?assertEqual([{attrib, "src", ends_with, "foo"}],   scan("[ src $= foo ]")),
    ?assertEqual([{attrib, "src", contains, "foo"}],    scan("[ src *= foo ]")).

invalid_attrib_test() ->
    ?assertError(syntax_error, scan("[]")),
    ?assertError(syntax_error, scan("[=]")),
    ?assertError(syntax_error, scan("[.a]")),
    ?assertError(syntax_error, scan("[:a]")),
    ?assertError(syntax_error, scan("[*]")),
    ?assertError(syntax_error, scan("[a b]")),
    ?assertError(syntax_error, scan("[a<b]")),
    ?assertError(syntax_error, scan("[a==c]")),
    ?assertError(syntax_error, scan("[a=^c]")),
    ?assertError(syntax_error, scan("[a=*c]")),
    ?assertError(syntax_error, scan("[a=b c]")).

pseudo_arg_test() ->
    ?assertEqual([{pseudo, 'nth-child', 4711}], scan(":nth-child(4711)")),
    ?assertEqual([{pseudo, 'nth-child', "foo"}], scan(":nth-child(foo)")).

pseudo_whitespace_test() ->
    ?assertEqual([{pseudo, 'nth-child', 4711}], scan(":nth-child( 4711 )")),
    ?assertEqual([{pseudo, 'nth-child', "foo"}], scan(":nth-child( foo )")).

invalid_pseudo_arg_test() ->
    ?assertError(syntax_error, scan(":nth-child()")),
    ?assertError(syntax_error, scan(":nth-child(*)")),
    ?assertError(syntax_error, scan(":nth-child([a])")),
    ?assertError(syntax_error, scan(":nth-child(:a)")),
    ?assertError(syntax_error, scan(":nth-child(a b)")),
    ?assertError(syntax_error, scan(":nth-child(a<b)")).

relations_test() ->
    ?assertEqual([{element, "aaa"}, {relation, ancestor}, {element, "bbb"}],
                 scan("aaa bbb")),
    ?assertEqual([{element, "aaa"}, {relation, parent}, {element, "bbb"}],
                 scan("aaa>bbb")),
    ?assertEqual([{element, "aaa"}, {relation, sibling}, {element, "bbb"}],
                 scan("aaa~bbb")),
    ?assertEqual([{element, "aaa"}, {relation, adjacent}, {element, "bbb"}],
                 scan("aaa+bbb")).

relations_whitespace_test() ->
    ?assertEqual([{element, "aaa"}, {relation, parent}, {element, "bbb"}],
                 scan("aaa > bbb")),
    ?assertEqual([{element, "aaa"}, {relation, sibling}, {element, "bbb"}],
                 scan("aaa ~ bbb")),
    ?assertEqual([{element, "aaa"}, {relation, adjacent}, {element, "bbb"}],
                 scan("aaa + bbb")).

together_test() ->
    ?assertEqual([{element, "aaa"},
                  {id, "bbb"},
                  {class, "ccc"},
                  {class, "ddd"},
                  {pseudo, 'eee'},
                  {attrib, "fff"},
                  {class, "ggg"},
                  {attrib, "hhh", equals, "iii"},
                  {pseudo, 'jjj', "hhh"}],
                 scan("aaa#bbb.ccc.ddd:eee[fff].ggg[hhh=iii]:jjj(hhh)")).

together_with_relations_test() ->
    ?assertEqual([{element, "aaa"},
                  {class, "bbb"},
                  {relation, parent},
                  {element, "ccc"},
                  {id, "ddd"},
                  {relation, ancestor},
                  {element, "*"},
                  {relation, ancestor},
                  {element, "*"},
                  {pseudo, 'eee'},
                  {relation, adjacent},
                  {attrib, "fff"}],
                 scan("aaa.bbb > ccc#ddd * *:eee + [fff]")).
