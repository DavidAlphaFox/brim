-module(brim_parse_transform_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("brim/include/brim.hrl").

transform_test() ->
    ?assertEqual({selector,
                  [{element, "aaa"},
                   {class, "bbb"},
                   {relation, parent},
                   {element, "*"},
                   {id, "ccc"},
                   {attrib, "ddd"}]},
                 sel("aaa.bbb > *#ccc[ddd]")).
