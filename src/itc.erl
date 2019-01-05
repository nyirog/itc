-module(itc).

%% API exports
-export([seed/0, norm/1, leq/2, fork/1, join/2, event/1]).

%%====================================================================
%% API functions
%%====================================================================

-type itc() :: {id:id(), event:event()}.

-spec seed() -> itc().
seed() -> {1, 0}.

-spec norm(itc()) -> itc().
norm({Id, Event}) -> {id:norm(Id), event:norm(Event)}.

-spec leq(itc(), itc()) -> boolean().
leq({_Id1, Event1}, {_Id2, Event2}) -> event:leq(Event1, Event2).

-spec fork(itc()) -> [itc()].
fork({Id, Event}) ->
    [Id1, Id2] = id:split(Id),
    [{Id1, Event}, {Id2, Event}].

-spec join(itc(), itc()) -> itc().
join({Id1, Event1}, {Id2, Event2}) -> {id:sum(Id1, Id2), event:join(Event1, Event2)}.

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

-spec fill(id:id(), event:event()) -> event:event().
fill(0, Event) -> Event;
fill(1, Event) -> event:max(Event);
fill(_, N) when erlang:is_integer(N) -> N;
fill({1, Iright}, {N, Left, Right}) ->
    Eright = fill(Iright, Right),
    event:norm({N, erlang:max(event:max(Left), event:min(Eright)), Eright});
fill({Ileft, 1}, {N, Left, Right}) ->
    Eleft = fill(Ileft, Left),
    event:norm({N, Eleft, erlang:max(event:max(Right), event:min(Eleft))});
fill({Ileft, Iright}, {N, Left, Right}) ->
    event:norm({N, fill(Ileft, Left), fill(Iright, Right)}).

-spec grow(id:id(), event:event()) -> {event:event(), non_neg_integer()}.
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
