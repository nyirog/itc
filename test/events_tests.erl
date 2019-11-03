-module(events_tests).
-include_lib("eunit/include/eunit.hrl").

get_last_tic_test() ->
    E = events:init(),
    F = events:append(E, {action, 0}),
    ?assertEqual({1, 1}, events:get_last_tic(F)).

filter_test() ->
    E = events:init(),
    F = events:append(E, {action, 0}),
    G = events:append(F, {action, 1}),
    ?assertEqual([1, 0], events:filter(G, action)).

list_unseen_events_test() ->
    E = events:init(),
    F = events:append(E, {action, 0}),
    G = events:append(F, {action, 1}),
    [L, R] = events:fork(G),
    H = events:append(L, {action, 10}),
    I = events:append(H, {action, 11}),
    S = events:append(R, {action, 20}),
    LastTic = events:get_last_tic(I),
    SyncFrom = events:get_last_seen_event_tic(S, LastTic),
    UnseenEvents = events:list_unseen_events(I, SyncFrom),
    ?assertEqual([11, 10], events:filter(UnseenEvents, action)).

merge_test() ->
    E = events:init(),
    F = events:append(E, {action, 0}),
    [L, R] = events:fork(F),
    H = events:append(L, {action, 10}),
    S = events:append(R, {action, 20}),
    LastTic = events:get_last_tic(H),
    SyncFrom = events:get_last_seen_event_tic(S, LastTic),
    UnseenEvents = events:list_unseen_events(H, SyncFrom),
    T = events:merge(S, UnseenEvents),
    ?assertEqual([10, 20, 0], events:filter(T, action)).

is_known_tic_test() ->
    E = events:init(),
    F = events:append(E, {action, 0}),
    Tic = events:get_last_tic(F),
    ?assertEqual(true, events:is_known_tic(F, Tic)),
    ?assertEqual(false, events:is_known_tic(E, Tic)).
