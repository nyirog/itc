-module(itc_tests).
-include_lib("eunit/include/eunit.hrl").

seed_test() ->
    ?assertEqual({1, 0}, itc:seed()).

norm_test() ->
    ?assertEqual({0, 1}, itc:norm({{0, 0}, 1})),
    ?assertEqual({1, 1}, itc:norm({{1, 1}, {1, 0, 0}})).

fork_test() ->
    ?assertEqual([{{1, 0}, 0}, {{0, 1}, 0}], itc:fork(itc:seed())).

join_test() ->
    [I, J] = itc:fork(itc:seed()),
    ?assertEqual({1, 0}, itc:join(I, J)).

event_test() ->
    S = itc:seed(),
    I = itc:event(S),
    ?assertEqual({1, 1}, I).

causality_test() ->
    S = itc:seed(),
    [F, G] = itc:fork(S),
    E = itc:event(F),
    J = itc:join(E, G),
    ?assertEqual({1, {0, 1, 0}}, J),

    ?assertEqual(true, itc:leq(F, E)),
    ?assertEqual(false, itc:leq(E, F)).
