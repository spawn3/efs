-module(ping).
-export([start/0,send/1,loop/0]).
-compile(export_all).

start() -> 
    spawn_link(ping, loop, []).

start1() -> 
    erlang:trace(all, true, [call,return_to]),
    erlang:trace_pattern({ping, '_', '_'}, true, [local]),
    Pid = spawn_link(ping, loop, []),
    ping:send(Pid).


send(Pid) ->
    Pid ! {self(), ping},
    receive pong -> pong end.

loop() ->
    receive 
        {Pid, ping} ->
            spawn(crash, do_not_exist, []),
            sum(1,2),
            Pid ! pong,
            loop()
    end.

sum(A, B) -> A + B.
