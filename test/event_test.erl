-module(event_test).
-include_lib("eunit/include/eunit.hrl").

leq_test() ->
    ?assertEqual(true, event:leq(2, 3)),
    ?assertEqual(true, event:leq(2, {3, 0, 1})),
    ?assertEqual(true, event:leq({2, 0, 1}, 3)),
    ?assertEqual(true, event:leq({2, 0, 1}, {3, 0, 1})).

norm_test() ->
    ?assertEqual(1, event:norm({1, 0, 0})),
    ?assertEqual(3, event:norm({1, 2, 2})),
    ?assertEqual({3, 0, 1}, event:norm({1, 2, 3})),
    ?assertEqual({3, 0, {0, 1, 1}}, event:norm({1, 2, {2, 1, 1}})).

join_test() ->
    ?assertEqual(3, event:join(2, 3)),
    ?assertEqual({3, 0, 1}, event:join({2, 1, 0}, {3, 0, 1})).
