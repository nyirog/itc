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
    event_server:add(a, 0),
    event_server:fork(a, b),
    ?assertEqual([0], event_server:list(b)),
    event_server:add(b, 1),
    ?assertEqual([1, 0], event_server:list(a)),
    event_server:stop(a),
    event_server:stop(b).

transitive_join_test() ->
    event_server:start_link(a),
    event_server:add(a, 0),
    event_server:fork(a, b),
    ?assertEqual([0], event_server:list(b)),
    event_server:add(b, 1),
    event_server:fork(b, c),
    event_server:add(c, 2),
    ?assertEqual([2, 1, 0], event_server:list(a)),
    event_server:stop(a),
    event_server:stop(b),
    event_server:stop(c).

resync_test() ->
    event_server:start_link(a),
    event_server:add(a, 0),
    {ok, Pid} = event_server:fork(a, b),
    event_server:add(b, 1),
    ?assertEqual([1, 0], event_server:list(a)), % check sync is ready
    true = erlang:suspend_process(Pid),
    event_server:add(a, 2),
    true = erlang:resume_process(Pid),
    timer:sleep(150),
    ?assertEqual([2, 1, 0], event_server:list(b)),
    event_server:stop(a),
    event_server:stop(b).
