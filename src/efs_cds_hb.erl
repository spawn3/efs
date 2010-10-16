-module(efs_cds_hb).
-export([start_link/1, stop/0, button/1]).
-export([init/1, code_change/4, handle_event/3, handle_info/3, 
        handle_sync_event/4, terminate/3]).
%% state
-export([locked/2, open/2, loop/2]).
-behaviour(gen_fsm).

-include("libefs.hrl").

start_link(Code) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Code, []).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

button(Digit) ->
    gen_fsm:send_event(?MODULE, {button, Digit}).


%%--------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------
init(Code) ->
    {ok, loop, {[], Code}, 1000}.

loop(timeout, {_SoFar, Code}) ->
    ?LOG([Code]),
    {next_state, loop, {[], Code}, 1000}.

locked({button, Digit}, {SoFar, Code}) ->
    ?LOG([[Digit|SoFar], Code]),
    case [Digit|SoFar] of
        Code ->
            ?LOG([Digit, SoFar, Code]),
            do_unlock(),
            {next_state, open, {[], Code}, 30000};
        Incomplete when length(Incomplete) < length(Code) ->
            {next_state, locked, {Incomplete, Code}};
        _Wrong ->
            {next_state, locked, {[], Code}}
    end.

open(timeout, State) ->
    do_lock(),
    {next_state, locked, State}.

handle_event(stop, _StateName, _StateData) ->
    {stop, normal, _StateData}.

code_change(_, _, _, _) -> ok.
handle_info(_, _, _) -> ok.
handle_sync_event(_, _, _, _) -> ok.
terminate(_, _, _) -> ok.

do_unlock() ->
    ?LOG([here]),
    ok.

do_lock() ->
    ?LOG([here]),
    ok.
