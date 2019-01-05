-module(id).

-export([norm/1]).

-export_type([id/0]).
-type id() :: 0 | 1 | {id(), id()}.

-spec norm(id()) -> id().
norm({0, 0}) -> 0;
norm({1, 1}) -> 1;
norm(I) when erlang:is_integer(I) -> I;
norm({I, J}) -> {norm(I), norm(J)}.
