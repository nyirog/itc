-module(id_tests).
-include_lib("eunit/include/eunit.hrl").

norm_test() ->
    ?assertEqual(0, id:norm({0, 0})),
    ?assertEqual(1, id:norm({1, 1})).

split_test() ->
    ?assertEqual([0, 0], id:split(0)),
    ?assertEqual([{1, 0}, {0, 1}], id:split(1)),
    ?assertEqual([{0, {1, 0}}, {0, {0, 1}}], id:split({0, 1})),
    ?assertEqual([{{1, 0}, 0}, {{0, 1}, 0}], id:split({1, 0})).
