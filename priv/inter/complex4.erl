-module(complex4).
-export([foo/1, bar/1]).

foo(X) ->
    call_cnode({foo, X}).
bar(Y) ->
    call_cnode({bar, Y}).

call_cnode(Msg) ->
    {any, 'cnode@192.168.0.11'} ! {call, self(), Msg},
    receive
	{cnode, Result} ->
	    Result
    end.
