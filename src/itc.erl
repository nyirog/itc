-module(itc).

%% API exports
-export([seed/0, norm/1, leq/2, fork/1, join/2]).

%%====================================================================
%% API functions
%%====================================================================

-type itc() :: {id:id(), event:event()}.

-spec seed() -> itc().
seed() -> {0, 1}.

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

%%====================================================================
%% Internal functions
%%====================================================================
