-module(efs_cds).
-behaviour(gen_server).
-export([init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).
-export([
        pwrite/5,
        pread/4,
        ping/1
    ]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("libefs.hrl").
-include("efs_cds.hrl").

-define(GR, {local, get_pname(MetaNo)}).
-define(GD, get_pname(MetaNo)).

%% state information
-record(stt, {
        metano,
        home
    }).

%%--------------------------------------------------------------
%% PC
%%--------------------------------------------------------------
start_link(MetaNo)->
    gen_server:start_link(?GR, ?MODULE, [MetaNo], []).

stop(MetaNo) ->
    gen_server:call(?GD, stop).

%%--------------------------------------------------------------
%% API
%%--------------------------------------------------------------
%% called by name
pwrite(Name, ChkID, ChkOff, Buflen, Buf) ->
    gen_server:call(Name, #cdp_pwrite_req{
            chkid  = ChkID,
            chkoff = ChkOff,
            buflen = Buflen,
            buf    = Buf
        }, ?CALL_TIMEOUT).

pread(Name, ChkID, ChkOff, Count) ->
    gen_server:call(Name, #cdp_pread_req{
            chkid  = ChkID,
            chkoff = ChkOff,
            count  = Count
        }, ?CALL_TIMEOUT).

shutdown(_Name) -> ok.
ping(_Name) -> ok.
clone(_Name, _ChkID) -> ok.

%%--------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------
init([MetaNo]) ->
    process_flag(trap_exit, true),
    %% 
    HOME = libefs:home_path(cds, MetaNo),
    _ = filelib:ensure_dir(HOME),
    _ = filelib:ensure_dir(HOME ++ ?DIR_DISK),
    _ = filelib:ensure_dir(HOME ++ ?DIR_JNL),
    _ = filelib:ensure_dir(HOME ++ ?DIR_CHUNK),

    case file:consult(HOME ++ "disk/diskinfo") of
        {ok, [DiskID]} -> ok;
        {error, _} ->
            DiskID = #diskid{id=0,version=0},
            ok
    end,

    put_pname(MetaNo),

    % diskjoin
    DiskStat = #diskstat{},
    case efs_mds:diskjoin({get_pname(MetaNo), node()}, #diskid{}, DiskStat) of
        #mdp_diskjoin_rep{diskid=NewDiskID, chkreport=REPT} = _Reply ->
            ?LOG([_Reply, DiskID, NewDiskID]),
            if 
                DiskID /= NewDiskID ->
                    _ = libefs:unconsult(HOME ++ "disk/diskinfo", [NewDiskID]); 
                true -> 
                    ok
            end,

            ?LOG("REPT"),
            case REPT of
                false -> ok;
                true -> 
                    cds_chkreport(get_pname(MetaNo), HOME ++ ?DIR_JNL, 1)
            end,

            register(get(jnl),  proc_lib:spawn_link(?MODULE, cds_jnl_init,        [MetaNo, HOME])),
            register(get(rept), proc_lib:spawn_link(?MODULE, cds_diskreport_init, [MetaNo, get_pname(MetaNo)])),
            register(get(hb),   proc_lib:spawn_link(?MODULE, cds_diskhb_init,     [MetaNo, get_pname(MetaNo)])),
            ?LOG("cds inited"),
            ok
    end,
    {ok, #stt{
            metano = MetaNo,
            home   = HOME
        }}.

handle_call(#cdp_pwrite_req{chkid=ChkID, chkoff=ChkOff, buflen=Buflen, buf=Buf} = _Request, _From, STT) ->
    _Request1 = _Request#cdp_pwrite_req{buf = <<>>},
    ?INFOMSG(_Request1),
    Reply = cds_pwrite(ChkID, ChkOff, Buflen, Buf, STT),
    {reply, Reply, STT};

handle_call(#cdp_pread_req{chkid=ChkID, chkoff=ChkOff, count=Count} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = cds_read(ChkID, ChkOff, Count, STT),
    {reply, Reply, STT};

handle_call({pwrite, _Buf, _Buflen} = _Request, _From, STT) ->
    ?INFOMSG(_Request),
    Reply = ok,
    {reply, Reply, STT}.

handle_cast(_Msg, State)->{noreply, State}.
handle_info(_Info, State)->{noreply, State}.
terminate(_Reason, _State)->ok.
code_change(_OldVsn, State, _Extra)->{ok, State}.

%%--------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------
cds_pwrite(ChkID, ChkOff, _Buflen, Buf, STT) ->
%    ?assertEqual(Buflen, size(Buf)),
    ChkFile = filename:join([STT#stt.home, ?DIR_CHUNK, libefs:id_to_list(ChkID)]),
    _ = libefs:pwrite(ChkFile, ?EFS_CDS_CHK_OFF+ChkOff, Buf),

    %% jnl
    ChkJnl = #md_chk_jnl{chkid=ChkID},
    cds_jnl_rpc(STT#stt.metano, ChkJnl),

    %% report
    cds_diskreport_rpc(STT#stt.metano, {add, ChkID}).

-spec cds_read(#chkid{}, integer(), integer(), #stt{}) -> {ok, binary()} | {error, eof}.

cds_read(ChkID, ChkOff, Count, STT) ->
    ChkFile = filename:join([STT#stt.home, ?DIR_CHUNK, libefs:id_to_list(ChkID)]),
    libefs:pread(ChkFile, ?EFS_CDS_CHK_OFF+ChkOff, Count).

chkjnl_report(_Name, [], _Max) ->
    ok;
chkjnl_report(Name, L, _Max) ->
    efs_mds:diskreport({Name, node()}, L),
    ok.

chkjnl_parse(Name, S, Offset, L) ->
    case file:pread(S, Offset, 28) of
        {ok, Buf} -> 
            ChkJnl = binary_to_chkjnl(Buf),
            L1 = [#chkreport{op=ChkJnl#md_chk_jnl.op, chkid=ChkJnl#md_chk_jnl.chkid}|L],
            efs_mds:diskreport({Name, node()}, L1),
            chkjnl_parse(Name, S, Offset+28, []),
            ok;
        eof ->
            ok;
        {error, _Reason} ->
            ok
    end.

cds_chkreport(Name, JnlFile) ->
    {ok, S} = file:open(JnlFile, [read,binary,raw]),
    chkjnl_parse(Name, S, 0, []),
    _ = file:close(S),
    
    ok.

cds_chkreport(Name, Dir, I) ->
    JnlFile = filename:join([Dir, integer_to_list(I)]),
    case filelib:is_regular(JnlFile) of
        false ->
            ok;
        true ->
            cds_chkreport(Name, JnlFile)
    end.

%% jnl
cds_jnl_rpc(_MetaNo, Msg) -> 
%    str_plus_number("efs_jnl_", _MetaNo) ! Msg.
    get(jnl) ! Msg.

cds_jnl_loop(HOME) ->
    receive 
        #md_chk_jnl{} = ChkJnl ->
            JnlFile = filename:join([HOME, ?DIR_JNL, "1"]),
            _ = chkjnl_append(JnlFile, ChkJnl),
            cds_jnl_loop(HOME);
        _Other -> 
            _Other,
            cds_jnl_loop(HOME)
    end.

cds_jnl_init(MetaNo, HOME) ->
    ?LOG(["cds jnl init", HOME]),
    put_pname(MetaNo),
    cds_jnl_loop(HOME).

%% diskhb
cds_diskhb_loop(MetaNo, Name) ->
    libefs:sleep(?DISK_HB_INTERVAL),

    cds_diskreport_rpc(MetaNo, report_to_mds),
    receive 
        _Other -> _Other
    after 0 -> ok
    end,

    efs_mds:diskhb(Name),

    cds_diskhb_loop(MetaNo, Name).

cds_diskhb_init(MetaNo, Name) ->
    ?LOG("cds hb init"),
    put_pname(MetaNo),
    cds_diskhb_loop(MetaNo, Name).

%% diskreport
cds_diskreport_rpc(_MetaNo, Msg) -> 
%    str_plus_number("cds_rept_", _MetaNo) ! Msg.
    get(rept) ! Msg.

cds_diskreport(_Name, []) ->
    ok;
cds_diskreport(Name, L) ->
    efs_mds:diskreport({Name, node()}, L),
    ok.

cds_diskreport_loop(Name, L) ->
    receive 
        {add, ChkID} ->
            cds_diskreport_loop(Name, [#chkreport{op=add, chkid=ChkID}|L]);
        {del, ChkID} ->
            cds_diskreport_loop(Name, [#chkreport{op=del, chkid=ChkID}|L]);
        report_to_mds ->
            cds_diskreport(Name, L),
            cds_diskreport_loop(Name, []);
        _Other ->
            ?LOG(_Other),
            cds_diskreport_loop(Name, L)
    end.

cds_diskreport_init(MetaNo, Name) ->
    ?LOG("cds report init"),
    put_pname(MetaNo),
    cds_diskreport_loop(Name, []).

-spec chkjnl_to_binary(#md_chk_jnl{}) -> <<_:224>>.

chkjnl_to_binary(#md_chk_jnl{magic=M,time=T,op=OP,chkid=#chkid{id=ID,version=CV},version=V}) ->
    OpCode = case OP of
        add -> 1;
        del -> 2
    end,
    <<M:32/little,T:32/little,OpCode:32/little,ID:64/little,CV:32/little,V:32/little>>.

-spec binary_to_chkjnl(binary()) -> #md_chk_jnl{}.

binary_to_chkjnl(Bin) ->
    <<M:32/little,T:32/little,OpCode:32/little,ID:64/little,CV:32/little,V:32/little>> = Bin,
    OP = case OpCode of
        1 -> add;
        2 -> del
    end,
    #md_chk_jnl{magic=M,time=T,op=OP,chkid=#chkid{id=ID,version=CV},version=V}.

chkjnl_append(File, ChkJnl) ->
    {ok, S} = file:open(File, [append,binary,raw]),
    file:write(S, chkjnl_to_binary(ChkJnl)),
    file:close(S).

str_plus_number(Str, N) ->
    list_to_atom(Str ++ integer_to_list(N)).

get_pname(MetaNo) ->
    str_plus_number("efs_cds_", MetaNo).

put_pname(MetaNo) ->
    put(hb,   str_plus_number("cds_hb_",   MetaNo)),
    put(jnl,  str_plus_number("cds_jnl_",  MetaNo)),
    put(rept, str_plus_number("cds_rept_", MetaNo)).
