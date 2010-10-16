-module(m).
-export([f/0]).
-compile(export_all).

-include_lib("stdlib/include/ms_transform.hrl").

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

f() ->
    receive 
        From when is_pid(From) ->
            Now = erlang:now(),
            From ! {self(), Now}
    end.

start() ->
    Pid = spawn(m,f,[]),

    ttb:tracer(),
    ttb:p(Pid,[call,send]),
    MS = dbg:fun2ms(fun(_) -> return_trace() end),
    ttb:tp(erlang,now,MS),

    Pid ! self(),

    ttb:stop().

