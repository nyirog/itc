-module(event_server_tests).
-include_lib("eunit/include/eunit.hrl").

list_test() ->
    event_server:start_link(a),
    event_server:add(a, 0),
    event_server:add(a, 1),
    ?assertEqual([1, 0], event_server:list(a)),
    event_server:stop(a).

join_test() ->
    event_server:start_link(a),
    event_server:start_link(b),
    event_server:add(a, 0),
    event_server:join(b, a),
    ?assertEqual([0], event_server:list(b)),
    event_server:add(b, 1),
    ?assertEqual([1, 0], event_server:list(a)),
    event_server:stop(a),
    event_server:stop(b).

transitive_join_test() ->
    event_server:start_link(a),
    event_server:start_link(b),
    event_server:start_link(c),
    event_server:add(a, 0),
    event_server:join(b, a),
    ?assertEqual([0], event_server:list(b)),
    event_server:add(b, 1),
    event_server:join(c, a),
    event_server:add(c, 2),
    ?assertEqual([2, 1, 0], event_server:list(a)),
    event_server:stop(a),
    event_server:stop(b),
    event_server:stop(c).