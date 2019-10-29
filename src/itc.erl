-module(itc).

%% API exports
-export([seed/0, norm/1, leq/2, fork/1, join/2, event/1]).
-export_type([itc/0]).

%%====================================================================
%% API functions
%%====================================================================

-type itc() :: {id(), event()}.

% @doc Initial ITC.
% @since 0.1.0
-spec seed() -> itc().
seed() -> {1, 0}.

% @private
-spec norm(itc()) -> itc().
norm({Id, Event}) -> {norm_id(Id), norm_event(Event)}.

% @doc Less or equal.
% @since 0.1.0
-spec leq(itc(), itc()) -> boolean().
leq({_Id1, Event1}, {_Id2, Event2}) -> leq_event(Event1, Event2).

% @doc The fork operation allows the cloning of the causal past of a stamp,
% resulting in a pair of stamps that have identical copies of the event
% component and distinct ids; fork({i, e}) = [{i1 , e}, {i2 , e}] such that
% i2 /= i1. Typically, i = i1 and i2 is a new id.
% @since 0.1.0
-spec fork(itc()) -> [itc()].
fork({Id, Event}) ->
    [Id1, Id2] = split(Id),
    [{Id1, Event}, {Id2, Event}].

% @doc This operation merges two stamps, producing a new one.
% If join({i1, e1}, {i2, e2}) = {i3, e3}, the resulting event
% component e3 should be such that leq(e1, e3) and leq(e2, e3).
% @since 0.1.0
-spec join(itc(), itc()) -> itc().
join({Id1, Event1}, {Id2, Event2}) -> {sum(Id1, Id2), join_event(Event1, Event2)}.

% @doc An event operation adds a new event to the event component, so that if
% {i, e'} results from event({i, e}) the causal ordering is such that e less than e'.
% @since 0.1.0
-spec event(itc()) -> itc().
event({Id, Event}) ->
    Filled = fill(Id, Event),
    if
        Filled /= Event -> {Id, Filled};
        true ->
            {Growed, _} = grow(Id, Event),
            {Id, Growed}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec fill(id(), event()) -> event().
fill(0, Event) -> Event;
fill(1, Event) -> max(Event);
fill(_, N) when erlang:is_integer(N) -> N;
fill({1, Iright}, {N, Left, Right}) ->
    Eright = fill(Iright, Right),
    norm_event({N, erlang:max(max(Left), min(Eright)), Eright});
fill({Ileft, 1}, {N, Left, Right}) ->
    Eleft = fill(Ileft, Left),
    norm_event({N, Eleft, erlang:max(max(Right), min(Eleft))});
fill({Ileft, Iright}, {N, Left, Right}) ->
    norm_event({N, fill(Ileft, Left), fill(Iright, Right)}).

-spec grow(id(), event()) -> {event(), non_neg_integer()}.
grow(1, N) when erlang:is_integer(N) -> {N + 1, 0};
grow(Id, N) when erlang:is_integer(N) ->
    {Event, C} = grow(Id, {N, 0, 0}),
    {Event, C + 1024};
grow({0, Iright}, {N, Left, Right}) ->
    {Eright, C} = grow(Iright, Right),
    {{N, Left, Eright}, C + 1};
grow({Ileft, 0}, {N, Left, Right}) ->
    {Eleft, C} = grow(Ileft, Left),
    {{N, Eleft,  Right}, C + 1};
grow({Ileft, Iright}, {N, Left, Right}) ->
    {Eleft, Cleft} = grow(Ileft, Left),
    {Eright, Cright} = grow(Iright, Right),
    if
        Cleft < Cright -> {{N, Eleft, Right}, Cleft + 1};
        true -> {{N, Left, Eright}, Cright + 1}
    end.

%%====================================================================
%% Interval tree id
%%====================================================================

-type id() :: 0 | 1 | {id(), id()}.

-spec norm_id(id()) -> id().
norm_id({0, 0}) -> 0;
norm_id({1, 1}) -> 1;
norm_id(I) when erlang:is_integer(I) -> I;
norm_id({I, J}) -> {norm_id(I), norm_id(J)}.

-spec split(id()) -> [id()].
split(0) -> [0, 0];
split(1) -> [{1, 0}, {0, 1}];
split({0, Id}) ->
    [Id1, Id2] = split(Id),
    [{0, Id1}, {0, Id2}];
split({Id, 0}) ->
    [Id1, Id2] = split(Id),
    [{Id1, 0}, {Id2, 0}];
split({Id1, Id2}) -> [{Id1, 0}, {0, Id2}].

-spec sum(id(), id()) -> id().
sum(0, I) -> I;
sum(I, 0) -> I;
sum(1, 1) -> 1;
sum(1, {LeftJ, RightJ}) -> norm_id({sum(1, LeftJ), sum(1, RightJ)});
sum({LeftI, RightI}, 1) -> norm_id({sum(LeftI, 1), sum(RightI, 1)});
sum({LeftI, RightI}, {LeftJ, RightJ}) -> norm_id({sum(LeftI, LeftJ), sum(RightI, RightJ)}).

%%====================================================================
%% Interval tree event
%%====================================================================

-type event() :: non_neg_integer() | {non_neg_integer(), event(), event()}.

-spec norm_event(event()) -> event().
norm_event(N) when erlang:is_integer(N) -> N;
norm_event({N, M, M}) when erlang:is_integer(N) andalso erlang:is_integer(M) -> N + M;
norm_event({N, Left, Right}) ->
    Min = erlang:min(min(Left), min(Right)),
    {N + Min, sink(Left, Min), sink(Right, Min)}.

-spec leq_event(event(), event()) -> boolean().
leq_event(N, M) when erlang:is_integer(N) andalso erlang:is_integer(M) ->
    N =< M;
leq_event(N, {M, _Left, _Right}) when erlang:is_integer(N) ->
    N =< M;
leq_event({N, Left, Right}, M) when erlang:is_integer(M) ->
    N =< M
    andalso leq_event(lift(Left, N), M)
    andalso leq_event(lift(Right, N), M);
leq_event({N, LeftN, RightN}, {M, LeftM, RightM}) ->
    N =< M
    andalso leq_event(lift(LeftN, N), lift(LeftM, M))
     andalso leq_event(lift(RightN, N), lift(RightM, M)).

-spec join_event(event(), event()) -> event().
join_event(N, M) when erlang:is_integer(N) andalso erlang:is_integer(M) -> erlang:max(N, M);
join_event(N, Event) when erlang:is_integer(N) -> join_event({N, 0, 0}, Event);
join_event(Event, M) when erlang:is_integer(M) -> join_event(Event, {M, 0, 0});
join_event(EventN = {N, _, _}, EventM = {M, _, _}) when N > M -> join_event(EventM, EventN);
join_event({N, LeftN, RightN}, {M, LeftM, RightM}) ->
    K = M - N,
    norm_event({N, join_event(LeftN, lift(LeftM, K)), join_event(RightN, lift(RightM, K))}).

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
