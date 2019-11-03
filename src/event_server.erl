-module(event_server).

-behaviour(gen_server).
-export([start_link/1, init/1, stop/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add/2, fork/2, list/1]).

%%==============================================================================
%% API
%%==============================================================================

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, Name, []).

stop(PidOrName) -> gen_server:stop(PidOrName).

add(Self, Action) ->
    gen_server:call(Self, {add, Action}).

list(Self) ->
    gen_server:call(Self, list).

fork(Self, Other) ->
    {ok, _Pid} = start_link(Other),
    gen_server:call(Other, {join, Self}).

%%------------------------------------------------------------------------------

init(Node) -> {ok, #{node => Node, events => events:init()}}.

terminate(_Reason, _State) -> ok.

handle_call({add, Action}, _From, State = #{events := Events, node := Self}) ->
    NewEvents = events:append(Events, {action, Action}),
    sync_nodes(NewEvents, Self),
    {reply, ok, State#{events := NewEvents}};

handle_call({fork, Other}, _From, State = #{events := Events, node := Self}) ->
    [LeftEvents, RightEvents] = events:fork(Events),
    JoinedEvents = events:append(LeftEvents, {join, Other}),
    sync_nodes(JoinedEvents, Self),
    {reply, RightEvents, State#{events := JoinedEvents}};

handle_call({join, Other}, _From, State = #{node := Self, events := [_]}) ->
    Events = gen_server:call(Other, {fork, Self}),
    JoinedEvents = events:append(Events, {join, Other}),
    {reply, ok, State#{events := JoinedEvents}};

handle_call({join, _}, _From, State = #{events := Events}) ->
    Error = {
        error, "Join is allowed only after init", events:filter(Events, action)
    },
    {reply, Error, State};

handle_call(list, _From, State = #{events := Events}) ->
    {reply, events:filter(Events, action), State};

handle_call({sync_from, SyncFrom}, _From, State = #{events := Events}) ->
    {reply, events:list_unseen_events(Events, SyncFrom), State};

handle_call(M,  _From, State) -> {reply, {error, "unknown message", M}, State}.

handle_cast({sync, Tic, Other}, State = #{events := Events}) ->
    SyncFrom = events:get_last_seen_event_tic(Events, Tic),
    UnseenEvents = gen_server:call(Other, {sync_from, SyncFrom}),
    MergedEvents = events:merge(Events, UnseenEvents),
    {noreply, State#{events := MergedEvents}};

handle_cast(_, State) -> {noreply, State}.

%%------------------------------------------------------------------------------

sync_nodes(Events, Self) ->
    lists:foreach(
        fun (Node) ->
            gen_server:cast(Node, {sync, events:get_last_tic(Events), Self})
        end,
        get_other_nodes(Events, Self)
    ).

get_other_nodes(Events, Self) ->
    Nodes = lists:usort(events:filter(Events, join)),
    lists:filter(fun (Other) -> Other =/= Self end, Nodes).
