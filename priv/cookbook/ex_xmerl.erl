-module(ex_xmerl).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-define(Doc, "<xml><person name=\"gj\" age=\"32\"><address>hello</address></person></xml>").

-define(LOG(X), io:format("[~p:~p] ~p~n", [?MODULE, ?LINE, X])).

start() ->
    scan(?Doc).

scan(Str) ->
    case xmerl_scan:string(Str) of
        {Root, _Tail} ->
            xpath(Root),
            parse(Root);
        _Other ->
            _Other
    end.

xpath(Doc) ->
    INCS = xmerl_xpath:string("./person/*", Doc),
    ?LOG([INCS]).


parse(_El = #xmlElement{parents=[], attributes=Attrs, content=Cont}) ->
    ?LOG([Attrs, Cont]),
    lists:foreach(fun parse/1, Cont);

parse(_El = #xmlElement{attributes=Attrs, content=Cont}) ->
    lists:foreach(fun parse/1, Attrs),
    lists:foreach(fun parse/1, Cont),
    ?LOG([Attrs, Cont]);

parse(#xmlAttribute{} = Attr) ->
    ?LOG([Attr]);

parse(#xmlText{} = Txt) ->
    ?LOG([Txt]).




