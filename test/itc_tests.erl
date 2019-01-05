-module(itc_tests).
-include_lib("eunit/include/eunit.hrl").

seed_test() ->
    ?assertEqual({0, 1}, itc:seed()).

norm_test() ->
    ?assertEqual({0, 1}, itc:norm({{0, 0}, 1})),
    ?assertEqual({1, 1}, itc:norm({{1, 1}, 1})),
    ?assertEqual({1, 1}, itc:norm({{1, 1}, {1, 0, 0}})).
