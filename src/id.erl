-module(id).

-export([norm/1, split/1, sum/2]).

-export_type([id/0]).
-type id() :: 0 | 1 | {id(), id()}.

-spec norm(id()) -> id().
norm({0, 0}) -> 0;
norm({1, 1}) -> 1;
norm(I) when erlang:is_integer(I) -> I;
norm({I, J}) -> {norm(I), norm(J)}.

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
sum({LeftI, RightI}, {LeftJ, RightJ}) -> norm({sum(LeftI, LeftJ), sum(RightI, RightJ)}).
