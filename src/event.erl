-module(event).

%% API exports
-export([norm/1, leq/2, join/2, max/1, min/1]).

-export_type([event/0]).
-type event() :: non_neg_integer() | {non_neg_integer(), event(), event()}.

%%====================================================================
%% API functions
%%====================================================================

-spec norm(event()) -> event().
norm(N) when erlang:is_integer(N) -> N;
norm({N, M, M}) when erlang:is_integer(N) andalso erlang:is_integer(M) -> N + M;
norm({N, Left, Right}) ->
    Min = erlang:min(min(Left), min(Right)),
    {N + Min, sink(Left, Min), sink(Right, Min)}.

-spec leq(event(), event()) -> boolean().
leq(N, M) when erlang:is_integer(N) andalso erlang:is_integer(M) -> N =< M;
leq(N, {M, _Left, _Right}) when erlang:is_integer(N) -> N =< M;
leq({N, Left, Right}, M) when erlang:is_integer(M) ->
    N =< M andalso leq(lift(Left, N), M) andalso leq(lift(Right, N), M);
leq({N, LeftN, RightN}, {M, LeftM, RightM}) ->
    N =< M andalso leq(lift(LeftN, N), lift(LeftM, M)) andalso leq(lift(RightN, N), lift(RightM, M)).

-spec join(event(), event()) -> event().
join(N, M) when erlang:is_integer(N) andalso erlang:is_integer(M) -> erlang:max(N, M);
join(N, Event) when erlang:is_integer(N) -> join({N, 0, 0}, Event);
join(Event, M) when erlang:is_integer(M) -> join(Event, {M, 0, 0});
join(EventN = {N, _, _}, EventM = {M, _, _}) when N > M -> join(EventM, EventN);
join({N, LeftN, RightN}, {M, LeftM, RightM}) ->
    K = M - N,
    norm({N, join(LeftN, lift(LeftM, K)), join(RightN, lift(RightM, K))}).

-spec min(event()) -> event().
min({N, Left, Right}) -> N + erlang:min(min(Left), min(Right));
min(N) -> N.

-spec max(event()) -> event().
max({N, Left, Right}) -> N + erlang:max(max(Left), max(Right));
max(N) -> N.


%%====================================================================
%% Internal functions
%%====================================================================

-spec lift(event(), non_neg_integer()) -> event().
lift(N, M) when erlang:is_integer(N)-> N + M;
lift({N, Left, Right}, M) -> {N + M, Left, Right}.

-spec sink(event(), non_neg_integer()) -> event().
sink(N, M) when erlang:is_integer(N) -> N - M;
sink({N, Left, Right}, M) -> {N - M, Left, Right}.
