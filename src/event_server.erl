-module(event_server).

-behaviour(gen_server).
-export([start_link/1, init/1, stop/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add/2, join/2, list/1]).

%%==============================================================================
%% API
%%==============================================================================

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, Name, []).

stop(PidOrName) -> gen_server:stop(PidOrName).

add(Self, Action) ->
    gen_server:call(Self, {add, Action}).

list(Self) ->
    gen_server:call(Self, list).

join(Self, Other) ->
    gen_server:call(Self, {join, Other}).

%%------------------------------------------------------------------------------

init(Node) -> {ok, #{node => Node, events => events:init()}}.

terminate(_Reason, _State) -> ok.

handle_call({add, Action}, _From, State = #{events := Events, node := Self}) ->
    NewEvents = events:append(Events, {action, Action}),
    Others = get_other_nodes(Events, Self),
    lists:foreach(fun (Node) -> update_node(NewEvents, Node) end, Others),
    {reply, ok, State#{events := events:append(Events, {action, Action})}};

handle_call(list, _From, State = #{events := Events}) ->
    {reply, events:filter(Events, action), State};

handle_call({sync, Tic}, _From, State = #{events := Events}) ->
    SyncFrom = events:get_last_seen_event_tic(Events, Tic),
    {reply, SyncFrom, State};

handle_call({fork, Other}, _From, State = #{events := Events, node := Self}) ->
    [LeftEvents, RightEvents] = events:fork(Events),
    JoinedEvents = events:append(LeftEvents, {join, Other}),
    Others = get_other_nodes(Events, Self),
    lists:foreach(fun (Node) -> update_node(JoinedEvents, Node) end, Others),
    {reply, RightEvents, State#{events := JoinedEvents}};

%% join is allowed only after init
handle_call({join, Other}, _From, State = #{node := Self, events := [_]}) ->
    Events = gen_server:call(Other, {fork, Self}),
    JoinedEvents = events:append(Events, {join, Other}),
    {reply, ok, State#{events := JoinedEvents}};

handle_call(_,  _From, State) -> {stop, shutdown, State}.

handle_cast({update, UnseenEvents}, State = #{events := Events}) ->
    {noreply, State#{events := events:merge(Events, UnseenEvents)}};

handle_cast(_, State) -> {stop, shutdown, State}.

%%------------------------------------------------------------------------------

update_node(Events, Node) ->
    LastTic = events:get_last_tic(Events),
    SyncFrom = gen_server:call(Node, {sync, LastTic}),
    UnseenEvents = events:list_unseen_events(Events, SyncFrom),
    gen_server:cast(Node, {update, UnseenEvents}).

get_other_nodes(Events, Self) ->
    Nodes = lists:usort(events:filter(Events, join)),
    lists:filter(fun (Other) -> Other =/= Self end, Nodes).
