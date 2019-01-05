-module(event).

-export([norm/1]).

-export_type([event/0]).
-type event() :: non_neg_integer() | {non_neg_integer(), event(), event()}.

-spec norm(event()) -> event().
norm(N) when erlang:is_integer(N) -> N;
norm({N, M, M}) when erlang:is_integer(N), erlang:is_integer(M) -> N + M;
norm({N, Left, Right}) ->
    Min = erlang:min(min(Left), min(Right)),
    {N + Min, sink(Left, Min), sink(Right, Min)}.

-spec min(event()) -> event().
min({N, Left, Right}) -> N + erlang:min(min(Left), min(Right));
min(N) -> N.

-spec max(event()) -> event().
max({N, Left, Right}) -> N + erlang:max(max(Left), max(Right));
max(N) -> N.

-spec lift(event(), non_neg_integer()) -> event().
lift(N, M) when erlang:is_integer(N)-> N + M;
lift({N, Left, Right}, M) -> {N + M, Left, Right}.

-spec sink(event(), non_neg_integer()) -> event().
sink(N, M) when erlang:is_integer(N) -> N - M;
sink({N, Left, Right}, M) -> {N - M, Left, Right}.
