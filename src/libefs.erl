-module(libefs).
-export([
        id_to_list/1,
        id_isvalid/1
    ]).

-compile(export_all).

-include("libefs.hrl").

%id_to_list(ID, Version) ->
%    integer_to_list(ID) ++ "_v" ++ integer_to_list(Version).

%id_to_list(#chkid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version);
%id_to_list(#fileid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version);
%id_to_list(#diskid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version).

id_to_list({_, ID, Version}) ->
    integer_to_list(ID) ++ "_v" ++ integer_to_list(Version).

id_isvalid({_, ID, Version}) ->
    if 
        ID =:= 0 orelse Version =:= 0 ->
            false;
        true ->
            true
    end.

%%--------------------------------------------------------------
%% File IO
%%--------------------------------------------------------------
unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).

pwrite(File, Off, Buf) ->
    {ok, S} = file:open(File, [read,write,raw,binary]),
    ok = file:pwrite(S, Off, Buf),
    file:close(S).

-spec pread(string(), integer(), integer()) -> {ok, binary()} | {error, eof}.

pread(File, Off, Count) ->
    {ok, S} = file:open(File, [read,binary,raw]),
    case file:pread(S, Off, Count) of
        {ok, Buf} ->
            file:close(S),
            {ok, Buf};
        _Other ->
            file:close(S),
            {error, eof}
    end.


%%--------------------------------------------------------------
%% utils
%%--------------------------------------------------------------
time() -> 
    Now = now(),
    element(1,Now)*1000000+element(2,Now).

sleep(T) ->
    receive 
    after T -> ok
    end.

-define(NYI(X), 
    (begin
        io:format("*** NYI ~p ~p ~p~n", [?MODULE, ?LINE, X]),
        exit(nyi)
    end)).

dump(File, Term) ->
    Out = File ++ ".tmp",
    io:format("** dumping to ~s~n", [Out]),
    {ok, S} = file:open(Out, [append]),
    io:format(S, "~p.~n", [Term]),
    file:close(S).

%%--------------------------------------------------------------
%% configure
%%--------------------------------------------------------------
home_path(cds, MetaNo) ->
    "/home/gj/git/efsroot/cds/" ++ integer_to_list(MetaNo) ++ "/"; 
home_path(mds, MetaNo) ->
    "/home/gj/git/efsroot/mds/" ++ integer_to_list(MetaNo) ++ "/". 

%%--------------------------------------------------------------
%% vector
%%--------------------------------------------------------------
vec_cal(Offset, MaxLen) ->
    {Offset div MaxLen, Offset rem MaxLen}.

vec_limit(Offset, Len, MaxLen) ->
    if 
        Offset >= MaxLen -> 0;
        Offset + Len >= MaxLen -> MaxLen - Offset;
        true -> Len
    end.

vec_update(NewOff, NewLen, OldOff, OldLen) ->
    NewEnd = NewOff + NewLen,
    OldEnd = OldOff + OldLen,

    ResOff = if 
        NewOff < OldOff -> NewOff;
        true            -> OldOff
    end,
    ResEnd = if
        NewEnd > OldEnd -> NewEnd;
        true            -> OldEnd
    end,
    {ResOff, ResEnd - ResOff}.

%%--------------------------------------------------------------
