-module(prop_itc).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/0, sample/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-record(state, {itc, forks}).

test() ->
    proper:quickcheck(?MODULE:prop_itc()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop_itc() ->
    ?FORALL(Commands, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    {History, State, Result} = run_commands(?MODULE, Commands),
                    ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~p~n",
                                        [History, State, Result]),
                              aggregate(command_names(Commands), Result =:= ok))
                end)
            ).

initial_state() -> #state{itc = itc:seed(), forks = [itc:seed()]}.

command(S) ->
    oneof([{call, itc, event, [S#state.itc]},
           {call, itc, fork, [S#state.itc]},
           {call, itc, join, [S#state.itc, elements(S#state.forks)]}]).

precondition(S, {call, itc, join, [_, _]}) -> erlang:length(S#state.forks) > 1;
precondition(_State, _Command) -> true.

next_state(S, _Result, {call, itc, event, [_]}) ->
    S#state{itc = itc:event(S#state.itc)};
next_state(S, _Result, {call, itc, fork, [_]}) ->
    [Left, Right] = itc:fork(S#state.itc),
    S#state{itc = Left, forks = [Right | S#state.forks]};
next_state(S, _Result, {call, itc, join, [Left, Right]}) ->
    S#state{itc = itc:join(Left, Right)}.

postcondition(S, {call, itc, event, _}, Result) ->
    itc:leq(S#state.itc, Result)
    andalso (not itc:leq(Result, S#state.itc))
    andalso itc:norm(Result)  =:= Result;
postcondition(S, {call, itc, fork, _}, [Left, Right]) ->
    itc:leq(S#state.itc, Left)
    andalso itc:leq(S#state.itc, Right)
    andalso itc:norm(Left) =:= Left
    andalso itc:norm(Right) =:= Right;
postcondition(S, {call, itc, join, _}, Result) ->
    itc:leq(S#state.itc, Result)
    andalso itc:norm(Result) =:= Result.
