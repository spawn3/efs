-module(efs).
-behaviour(gen_server).
-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).
-compile(export_all).

-include("efs_test.hrl").
-include("libefs.hrl").
-include("efs.hrl").

-define(GR, {local, ?MODULE}).
-define(GD, ?MODULE).

%% state information
-record(stt, {
        maxfd
    }).

%%--------------------------------------------------------------
%% PC
%%--------------------------------------------------------------
start_link() ->
    gen_server:start_link(?GR, ?MODULE, [], []).

stop() ->
    gen_server:call(?GD, stop).

%%--------------------------------------------------------------
%% API 
%%--------------------------------------------------------------
create(Path) -> 
    gen_server:call(?GD, #create{path=Path}, ?CALL_TIMEOUT).

open(Path) ->
    gen_server:call(?GD, #open{path=Path}, ?CALL_TIMEOUT).

close(Fd) ->
    gen_server:call(?GD, #close{fd=Fd}, ?CALL_TIMEOUT).

pwrite(Fd, Offset, Buflen, Buf) ->
    gen_server:call(?GD, #pwrite{
            fd     = Fd,
            offset = Offset,
            buflen = Buflen,
            buf    = Buf
        }, ?CALL_TIMEOUT).

pread(Fd, Offset, Buflen) ->
    gen_server:call(?GD, #pread{
            fd     = Fd,
            offset = Offset,
            buflen = Buflen
        }, ?CALL_TIMEOUT).

fsync(Fd) ->
    gen_server:call(?GD, #fsync{fd=Fd}, ?CALL_TIMEOUT).

truncate(#fileid{} = FID, Length) ->
    gen_server:call(?GD, #fid_truncate{fid=FID, len=Length}, ?CALL_TIMEOUT);
truncate(Path, Length) ->
    gen_server:call(?GD, #truncate{path=Path, len=Length}, ?CALL_TIMEOUT).

fstat(Fd) ->
    gen_server:call(?GD, #fstat{fd=Fd}, ?CALL_TIMEOUT).

stat(Path) ->
    gen_server:call(?GD, #stat{path=Path}, ?CALL_TIMEOUT).

show() ->
    gen_server:call(?GD, show, ?CALL_TIMEOUT).

mkdir(Path) ->
    gen_server:call(?GD, #mkdir{path=Path}, ?CALL_TIMEOUT).

readdir(Path) ->
    gen_server:call(?GD, #readdir{path=Path}, ?CALL_TIMEOUT).


%%--------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------
init([]) ->
    _ = ets:new(efs_fdtable, [set, named_table, protected]),

    {ok, #stt{
            maxfd = 1
        }}.

handle_call(#create{path=Path} = _Request, _From, #stt{maxfd=MaxFd}=STT) ->
    ?INFOMSG(_Request),
    Efd = 
    case efs_mds:create(Path, 8#644) of
        {ok, #mdp_create_rep{fid=FID, fmeta=Fmeta}} ->
            case open1(Path, FID, Fmeta, MaxFd) of
                {ok, MaxFd} ->
                    MaxFd;
                {error, _Reason} ->
                    ?FD_ERR
            end;
        {error, _Reason} ->
            ?FD_ERR
    end,
    case Efd of
        ?FD_ERR ->
            {reply, {error, Efd}, STT};
        _ ->
            {reply, {ok, Efd}, STT#stt{maxfd=Efd+1}}
    end;

handle_call(#open{path=Path} = _Request, _From, #stt{maxfd=MaxFd} = STT) ->
    ?INFOMSG(_Request),
    Reply = case efs_mds:open(Path) of
        {ok, #mdp_open_rep{fid=FID, fmeta=Fmeta}} ->
            case open1(Path, FID, Fmeta, MaxFd) of
                {ok, MaxFd} ->
                    MaxFd;
                {error, _Reason} ->
                    ?FD_ERR
            end;
        {error, _Reason} ->
            ?FD_ERR
    end,
    case Reply of
        ?FD_ERR ->
            {reply, {error, Reply}, STT};
        _ ->
            {reply, {ok, Reply}, STT#stt{maxfd=Reply+1}}
    end;

handle_call(#close{fd=Fd} = _Request, _From, #stt{} = STT) ->
    ?INFOMSG(_Request),
    Reply = ets:delete(efs_fdtable, Fd),
    {reply, Reply, STT};

handle_call(#pwrite{fd=Fd, offset=Offset, buflen=Buflen, buf=Buf} = _Request, _From, #stt{} = STT) ->
    _Request1 = _Request#pwrite{buf = <<>>},
    ?INFOMSG(_Request1),
    Reply = pwrite1(Fd, Offset, Buflen, Buf),
    {reply, Reply, STT};

handle_call(#pread{fd=Fd, offset=Offset, buflen=Buflen} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = pread1(Fd, Offset, Buflen, <<>>),
    {reply, Reply, STT};

handle_call(#fstat{fd=Fd} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = Fd,
    {reply, Reply, STT};

handle_call(#stat{path=Path} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = efs_mds:getattr(Path),
    {reply, Reply, STT};

handle_call(#truncate{path=Path, len=Length} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = efs_mds:truncate(Path, Length),
    {reply, Reply, STT};

handle_call(#fid_truncate{fid=FID, len=Length} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = efs_mds:truncate(FID, Length),
    {reply, Reply, STT};

handle_call(#mkdir{path=Path} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = efs_mds:mkdir(Path),
    {reply, Reply, STT};

handle_call(#readdir{path=Path} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = efs_mds:readdir(Path),
    {reply, Reply, STT};

handle_call(show = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = ok,
    {reply, Reply, STT};

handle_call(_Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = ok,
    {reply, Reply, STT}.

handle_cast(_Msg, STT)->{noreply, STT}.

handle_info(_Info, STT)->{noreply, STT}.

terminate(_Reason, _State)->ok.

code_change(_OldVsn, STT, _Extra)->{ok, STT}.


%%--------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------
open1(Path, FID, Fmeta, MaxFd) ->
    case ets:lookup(efs_fdtable, MaxFd) of
        [] ->
            Efile = #efs_file{path=Path, fid=FID, fmeta=Fmeta},
            ets:insert(efs_fdtable, {MaxFd, Efile}),
            {ok, MaxFd};
        [_] ->
            {error, ?FD_ERR}
    end.

fmeta_chklist_resize(A, NewSize, NewSize) ->
    A;
fmeta_chklist_resize(A, I, NewSize) ->
    A1 = array:set(I, #md_chk{}, A),
    fmeta_chklist_resize(A1, I+1, NewSize).

fmeta_chklist_resize(A, NewSize) ->
    Size = array:size(A),
    if 
        Size < NewSize ->
            A1 = array:resize(NewSize, A),
            fmeta_chklist_resize(A1, Size, NewSize);
        true ->
            A
    end.

-spec chkload1(#chkid{}, integer(), term()) -> {#chkid{}, list(), term()}.

chkload1(ChkID, ChkNo, CL) ->
    %    ?assert(libefs:is_id(ChkID)),

    case array:get(ChkNo, CL) of
        undefined ->
            {_NewChkID, Dlist} = efs_mds:chkload(ChkID, ChkNo),
            CL1 = array:set(ChkNo, #efs_chk{loaded=true, disks=Dlist}, CL);
        #efs_chk{loaded=Loaded} = Chk ->
            {_NewChkID, Dlist} = case Loaded of
                false ->
                    efs_mds:chkload(ChkID, ChkNo);
                true -> 
                    {ChkID, Chk#efs_chk.disks}
            end,
            CL1 = array:set(ChkNo, Chk#efs_chk{loaded=true, disks=Dlist}, CL)
    end,
    {ChkID, Dlist, CL1}.

-spec chkload(integer(), integer()) -> 
    {#chkid{}, list(), #md_file{}} | 
    {error, any()}.

chkload(Fd, Offset) ->
    case ets:lookup(efs_fdtable, Fd) of
        [{_, #efs_file{fid=_FID, fmeta=Fmeta, chklist=CL} = Efile}] ->
            {ChkNo, _} = libefs:vec_cal(Offset, Fmeta#md_file.chklen),
            case array:get(ChkNo, Fmeta#md_file.chklist) of
                #md_chk{chkid=ChkID} ->
                    {NewChkID, Dlist, CL1} = chkload1(ChkID, ChkNo, CL),
                    ets:insert(efs_fdtable, {Fd, Efile#efs_file{chklist=CL1}}),
                    {NewChkID, Dlist, Fmeta};
                undefined ->
                    {error, chkload}
            end;
        _ -> 
            {error, chkload}
    end.


chkget(Fd, Offset) ->
    case ets:lookup(efs_fdtable, Fd) of
        [{_, #efs_file{fid=FID, fmeta=Fmeta, chklist=CL} = Efile}] ->
            ChkNo = Offset div Fmeta#md_file.chklen,
            MDCL = 
            case array:get(ChkNo, Fmeta#md_file.chklist) of
                #md_chk{} ->
                    Fmeta#md_file.chklist;
                undefined ->
                    array:set(ChkNo, #md_chk{}, Fmeta#md_file.chklist)
            end,

            #md_chk{chkid=ChkID} = array:get(ChkNo, MDCL),
            case libefs:is_id(ChkID) of
                false ->
                    {NewChkID, Dlist} = efs_mds:chkget(ChkID, ChkNo, ?CHK_REP), 
                    CL1 = array:set(ChkNo, #efs_chk{loaded=true, disks=Dlist}, CL);
                true -> 
                    {NewChkID, Dlist, CL1} = chkload1(ChkID, ChkNo, CL)
            end,

            MDCL1 = array:set(ChkNo, #md_chk{chkid=NewChkID}, MDCL),
            Fmeta1 = Fmeta#md_file{chklist=MDCL1},
            ets:insert(efs_fdtable, {Fd, Efile#efs_file{fmeta=Fmeta1, chklist=CL1}}),

            {ok, FID, NewChkID, Fmeta1#md_file.chklen, Dlist};
        [_Other] ->
            ?LOG(_Other),
            {error, fd};
        [] -> 
            {error, fd}
    end.


send([], FID, ChkID, ChkNo, ChkOff, Buflen, _) ->
    efs_mds:fsync(FID, ChkID, ChkNo, ChkOff, Buflen),
    ok;
send([H|T], FID, ChkID, ChkNo, ChkOff, Buflen, Buf) ->
    efs_cds:pwrite(H, ChkID, ChkOff, Buflen, Buf),
    send(T, FID, ChkID, ChkNo, ChkOff, Buflen, Buf).

pwrite1(Fd, Offset, Buflen, Buf) ->
    case chkget(Fd, Offset) of
        {ok, FID, ChkID, MaxChkLen, Dlist} ->
            {ChkNo, ChkOff} = libefs:vec_cal(Offset, MaxChkLen),
            Len = libefs:vec_limit(ChkOff, Buflen, MaxChkLen),

            if 
                Buflen =:= Len ->
                    send(Dlist, FID, ChkID, ChkNo, ChkOff, Len, Buf);
                true ->
                    <<Buf1:Len/binary,Buf2/binary>> = Buf,
                    send(Dlist, FID, ChkID, ChkNo, ChkOff, Len, Buf1),
                    pwrite1(Fd, Offset + Len, Buflen - Len, Buf2)
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

-spec pread1(integer(), integer(), integer(), binary()) -> 
    {ok, binary()} | 
    {error, any()}.

pread1(_, _, 0, <<>>) ->
    {error, nodata};
pread1(_, _, 0, Acc) ->
    {ok, Acc};
pread1(Fd, Offset, Buflen, Acc) ->
    %    ?LOG([Fd, Offset, Buflen]),

    case chkload(Fd, Offset) of
        {ChkID, [_H|_] = Dlist, #md_file{flen=Flen, chklen=MaxChkLen} = Fmeta} ->
            {ChkNo, ChkOff} = libefs:vec_cal(Offset, MaxChkLen),

            Buflen1 = libefs:vec_limit(Offset, Buflen, Flen),
            Buflen2 = libefs:vec_limit(ChkOff, Buflen1, MaxChkLen),

            if 
                Buflen2 > 0 ->
                    Cmeta = array:get(ChkNo, Fmeta#md_file.chklist),
                    ChkID = Cmeta#md_chk.chkid,
                    N = random:uniform(length(Dlist)),
                    case efs_cds:pread(lists:nth(N, Dlist), ChkID, ChkOff, Buflen2) of
                        {ok, Buf} ->
                            ReadLen = byte_size(Buf),
                            pread1(Fd, Offset + ReadLen, Buflen - ReadLen, 
                                <<Acc/binary, Buf/binary>>);
                        _Other -> 
                            _Other
                    end;
                true ->
                    pread1(Fd, Offset, 0, Acc)
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

