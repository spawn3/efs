-module(efs_fdtable).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).
-compile(export_all).

-record(state, {table, maxfd}).

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------
%% API
%%-----------------------------------------------------------
fdtable_add(Path) ->
    gen_server:call(?MODULE, {ft_add, Path}).

fdtable_del(Fd) ->
    gen_server:call(?MODULE, {ft_del, Fd}).

fdtable_list() ->
    gen_server:call(?MODULE, ft_list).

%%-----------------------------------------------------------
%% callbacks
%%-----------------------------------------------------------
init([]) ->
    {ok, #state{table=ets:new(fdtable, [set]), maxfd=0}}.

handle_call({ft_add, Path}, _From, #state{table=Tbl, maxfd=Maxfd} = State) ->
    Maxfd1 = Maxfd + 1,
    ets:insert(Tbl, {Maxfd1, Path}),
    Reply = {ft_add, Maxfd1},
    State1 = State#state{maxfd=Maxfd1},
    {reply, Reply, State1};
handle_call({ft_del, Fd}, _From, #state{table=Tbl} = State) ->
    ets:delete(Tbl, Fd),
    Reply = {ft_del, ok},
    {reply, Reply, State};
handle_call(ft_list, _From, State) ->
    Reply = State,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%-----------------------------------------------------------
%% Asynchronous Call
%%-----------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------




