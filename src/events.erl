-module(events).

-export([init/0, append/2, fork/1, merge/2]).
-export([get_last_seen_event_tic/2, list_unseen_events/2, get_last_tic/1, filter/2]).

-type action() :: any().
-type tic() :: itc:itc().
-type event() :: {tic(), action()}.
-type events() :: nonempty_list(event()).

-spec list_unseen_events(events(), tic()) -> list(event()).
list_unseen_events(Events, Tic) ->
    Cmp = fun ({A, _}, {B, _}) -> itc:leq(B, A) end,
    lists:sort(Cmp, [Event || {EventTic, _} = Event <- Events, not itc:leq(EventTic, Tic)]).

-spec get_last_seen_event_tic(events(), tic()) -> tic().
get_last_seen_event_tic(Events, Tic) ->
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
append(Events, Action) ->
    [{itc:event(get_last_tic(Events)), Action}|Events].

-spec fork(events()) -> list(events()).
fork(Events) ->
    [Tic, Toc] = itc:fork(get_last_tic(Events)),
    [[{Tic, fork} | Events], [{Toc, fork}| Events]].

-spec merge(events(), list(event())) -> events().
merge(Events, []) -> Events;
merge(Events, [Event = {_, merge}]) -> [Event | Events];
merge(Events, UnseenEvents) ->
    Tic = itc:event(itc:join(get_last_tic(Events), get_last_tic(UnseenEvents))),
    [{Tic, merge} | UnseenEvents] ++ Events.

-spec get_last_tic(events()) -> tic().
get_last_tic([{Tic, _} | _]) -> Tic.

-spec filter(events(), atom()) -> list(any()).
filter(Events, ActionKey) ->
    lists:reverse(filter(Events, ActionKey, [])).

filter([], _ActionKey, FilteredActions) -> FilteredActions;
filter([{_, {ActionKey, Action}} | Tail], ActionKey, FilteredActions) ->
    filter(Tail, ActionKey, [Action|FilteredActions]);
filter([_ | Tail], ActionKey, FilteredActions) ->
    filter(Tail, ActionKey, FilteredActions).
