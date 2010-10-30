-module(efs_mds).
-behaviour(gen_server).
-export([
        start_link/1,
        stop/0,
        init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2, 
        code_change/3]).
-export([
        diskjoin/3,
        mkdir/1,
        truncate/2
    ]).
-compile(export_all).

-include("efs_test.hrl").
-include("libefs.hrl").
-include("efs_mds.hrl").

-define(GD, {global, ?MODULE}).

%% state information
-record(stt, {
        metano,
        home,
        version,
        %% ID
        diskid,
        fid,
        chkid
    }).


%%--------------------------------------------------------------
%% PC
%%--------------------------------------------------------------
start_link(MetaNo)->
    gen_server:start_link(?GD, ?MODULE, [MetaNo], []).

stop() ->
    gen_server:call(?GD, stop).

%%--------------------------------------------------------------
%% API
%%--------------------------------------------------------------

%% @spec diskjoin(cdsname(), #diskid{}, #diskstat{}) -> any()
%% @doc 
%% @see diskjoin/3
-spec diskjoin(cdsname(), #diskid{}, #diskstat{}) -> any().

diskjoin(Name, DiskID, DiskStat) ->
    gen_server:call(?GD, #mdp_diskjoin_req{
            name     = Name,
            diskid   = DiskID,
            diskstat = DiskStat
        }, ?CALL_TIMEOUT).

diskhb(Name) ->
    gen_server:call(?GD, #mdp_diskhb_req{
            name = Name
        }, ?CALL_TIMEOUT).

diskreport(Name, L) ->
    gen_server:call(?GD, #mdp_diskreport_req{
            name = Name,
            bin  = term_to_binary(L)
        }, ?CALL_TIMEOUT).

%% super ops
ping() -> ok.
statvfs() -> ok.

%% node
getattr(Path) -> 
    gen_server:call(?GD, #mdp_getattr_req{path = Path}, ?CALL_TIMEOUT).

truncate(#fileid{} = FID, Length) ->
    gen_server:call(?GD, #mdp_fidtruncate_req{
            fid = FID,
            len = Length
        }, ?CALL_TIMEOUT);
truncate(Path, Length) -> 
    gen_server:call(?GD, #mdp_truncate_req{
            path = Path,
            len  = Length
        }, ?CALL_TIMEOUT).

rename() -> ok.
chmod() -> ok.
chown() -> ok.
unlink() -> ok.

%% fid
fid_open() -> ok.
fid_getattr() -> ok.
fid_rename() -> ok.
fid_chmod() -> ok.
fid_chown() -> ok.

%% file ops
create(Path, Mode)->
    gen_server:call(?GD, #mdp_create_req{
            path = Path,
            mode = Mode,
            fid  = #fileid{}
        }, ?CALL_TIMEOUT).

open(Path)->
    gen_server:call(?GD, #mdp_open_req{path=Path}, ?CALL_TIMEOUT).

fsync(FID, ChkID, ChkNO, ChkOff, ChkLen)->
    gen_server:call(?GD, #mdp_fsync_req{
            fid    = FID,
            chkid  = ChkID,
            chkno  = ChkNO,
            chkoff = ChkOff,
            chklen = ChkLen
        }, ?CALL_TIMEOUT).

%% dir
mkdir(Path) -> 
    gen_server:call(?GD, #mdp_mkdir_req{path=Path}, ?CALL_TIMEOUT).

readdir(Path) ->
    gen_server:call(?GD, #readdir{path=Path}, ?CALL_TIMEOUT).

rmdir() -> ok.
readdirplus() -> ok.

%% chk ops
chkget(ChkID, ChkNO, ChkRep)->
    gen_server:call(?GD, #mdp_chkget_req{
            chkid  = ChkID,
            chkno  = ChkNO,
            chkrep = ChkRep,
            chklen = 0
        }, ?CALL_TIMEOUT).

chkload(ChkID, ChkNO)->
    gen_server:call(?GD, #mdp_chkload_req{
            chkid  = ChkID,
            chkno  = ChkNO,
            chklen = 0
        }, ?CALL_TIMEOUT).

%% symlink ops
symlink() -> ok.
readlink() -> ok.
link() -> ok.

%% logical volume management
lvm_create() -> ok.
lvm_set() -> ok.
lvm_list() -> ok.

%%--------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------
init([MetaNo])->
    HOME = libefs:home_path(mds, MetaNo),
    _ = filelib:ensure_dir(HOME ++ ?DIR_ROOT),
    _ = filelib:ensure_dir(HOME ++ ?DIR_FILEID),
    _ = filelib:ensure_dir(HOME ++ ?DIR_LVM),

    %% for chk location cache
    _ = filelib:ensure_dir(HOME ++ ?DIR_DISK),
    _ = filelib:ensure_dir(HOME ++ ?DIR_CHUNK),

    efs_mds_diskpool:new(),
    efs_mds_chkpool:new(),

    {ok, #stt{
            metano   = MetaNo,
            home     = HOME,
            version  = libefs:time(),
            diskid   = 1,
            fid      = 1,
            chkid    = 1
        }}.

%% client
handle_call(#mdp_create_req{path=Path} = _Msg, _From, STT)->
    ?INFOMSG(_Msg),
    %% existed?
    File = filename:join([STT#stt.home, ?DIR_ROOT, Path]),
    case filelib:is_regular(File) of
        false -> 
            %% FID
            FID = #fileid{id=STT#stt.fid, version=STT#stt.version},

            IdFile = filename:join([STT#stt.home, ?DIR_FILEID, libefs:id_to_list(FID)]),
            Meta = #md_file{},

            _ = libefs:unconsult(IdFile, [Meta]),
            _ = libefs:unconsult(File, [FID]),

            NewSTT = STT#stt{fid=FID#fileid.id+1},
            Reply = {ok, #mdp_create_rep{fid=FID, fmeta=Meta}},
            {reply, Reply, NewSTT};
        true -> 
            Reply = {error, existed},
            {reply, Reply, STT}
    end;

handle_call(#mdp_open_req{path=Path} = _Msg, _From, STT)->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:open(STT#stt.home, Path),
    {reply, Reply, STT};

%% chk operations
handle_call(#mdp_chkget_req{chkid=ChkID, chkrep=ChkRep} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    NewChkID = case libefs:is_id(ChkID) of
        false -> #chkid{id=STT#stt.chkid, version=STT#stt.version};
        true  -> ChkID
    end,

    Dlist = efs_mds_diskpool:chkget(NewChkID, ChkRep),
    Reply = {NewChkID, Dlist},
    {reply, Reply, STT#stt{chkid=NewChkID#chkid.id+1}};

handle_call(#mdp_chkload_req{chkid=ChkID} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Dlist = efs_mds_chkpool:chkload(ChkID),
    Reply = {ChkID, Dlist},
    {reply, Reply, STT};

%% CDS
handle_call(#mdp_diskjoin_req{name=Name, diskid=DiskID, diskstat=DiskStat} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    case libefs:is_id(DiskID) of
        false ->
            NewDiskID = #diskid{id=STT#stt.diskid, version=STT#stt.version},
            NewSTT = STT#stt{diskid=STT#stt.diskid+1};
        true ->
            NewDiskID = DiskID,
            NewSTT = STT
    end,
    ?LOG({DiskID, NewDiskID}),

    efs_mds_diskpool:add(NewDiskID, Name, DiskStat),

    Reply = #mdp_diskjoin_rep{diskid=NewDiskID, chkreport=true},
    {reply, Reply, NewSTT};

handle_call(#mdp_diskreport_req{name=Disk, bin=Bin} = _Msg, _From, #stt{} = STT) ->
    _Request1 = _Msg#mdp_diskreport_req{bin = <<>>},
    ?INFOMSG(_Request1),
    L = binary_to_term(Bin),
    Reply = efs_mds_chkpool:report(Disk, L),
    {reply, Reply, STT};

handle_call(#mdp_diskhb_req{name=_Name} = _Msg, _From, STT) ->
%    ?INFOMSG(_Msg),
    Reply = ok,
    {reply, Reply, STT};

handle_call(#mdp_fsync_req{fid=FID, chkid=ChkID, chkno=ChkNo, 
        chkoff=NewChkOff, chklen=NewChkLen} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:fsync(STT#stt.home, FID, ChkID, ChkNo, NewChkOff, NewChkLen),
    {reply, Reply, STT};

handle_call(#mdp_getattr_req{path=Path} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:getattr(STT#stt.home, Path),
    {reply, Reply, STT};

handle_call(#mdp_truncate_req{path=Path, len=Length} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:truncate(STT#stt.home, Path, Length),
    {reply, Reply, STT};

handle_call(#mdp_fidtruncate_req{fid=FID, len=Length} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:truncate(STT#stt.home, FID, Length),
    {reply, Reply, STT};

handle_call(#mdp_mkdir_req{path=Path} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:mkdir(STT#stt.home, Path),
    {reply, Reply, STT};

handle_call(#readdir{path=Path} = _Msg, _From, STT) ->
    ?INFOMSG(_Msg),
    Reply = efs_mds_model:readdir(STT#stt.home, Path),
    {reply, Reply, STT};

handle_call(_Msg, _From, STT)->
    ?INFOMSG(_Msg),
    Reply = ok,
    {reply, Reply, STT}.

handle_cast(_Msg, State)->{noreply, State}.
handle_info(_Info, State)->{noreply, State}.
terminate(_Reason, _State)->ok.
code_change(_OldVsn, State, _Extra)->{ok, State}.

%%--------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------

%% diskpool
%% chkpool
%filelib:ensure_dir(File),
%io:format("create ~p~n", [File]),
%{ok, S} = file:open(File, [write]),
%io:format(S, "~p~n", [File]),
%file:close(S),
