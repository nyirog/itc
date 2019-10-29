-module(store).

-export([init/0, append/2, fork/1, update/2]).
-export([get_last_seen_event_clock/2, list_unseen_events/2]).

-type action() :: any().
-type tic() :: itc:itc().
-type event() :: {tic(), action()}.
-type events() :: nonempty_list(event()).

-spec list_unseen_events(events(), tic()) -> list(event()).
list_unseen_events(Events, Tic) ->
    Cmp = fun ({Toc, _}, {Tuc, _}) -> itc:leq(Tuc, Toc) end,
    lists:sort(Cmp, [Event || {EventTic, _} = Event <- Events, not itc:leq(EventTic, Tic)]).

-spec get_last_seen_event_clock(events(), tic()) -> tic().
get_last_seen_event_clock(Events, Tic) ->
    Tics = [EventTic || {EventTic, _} <- Events, itc:leq(EventTic, Tic)],
    Cmp = fun (A, B) -> itc:leq(B, A) end,
    case Tics of
        [] -> itc:seed();
        [Toc] -> Toc;
        Tocs -> erlang:hd(lists:sort(Cmp, Tocs))
    end.

-spec init() -> events().
init() -> [{itc:seed(), init}].

-spec append(events(), action()) -> events().
append([{Tic, _} | _] = Events, Action) -> [{itc:event(Tic), Action}|Events].

-spec fork(events()) -> list(events()).
fork([{Tic, _} | _] = Events) ->
    [Toc, ForkedToc] = itc:fork(Tic),
    [[{Toc, fork} | Events], [{ForkedToc, fork}| Events]].


-spec update(events(), list(event())) -> events().
update(Events, []) -> Events;
update([{Tic, _} | _] = Events, [{Toc, _} | _] = NewEvents) ->
    UpdateTic = itc:event(itc:join(Tic, Toc)),
    [{UpdateTic, update} | NewEvents] ++ Events.
