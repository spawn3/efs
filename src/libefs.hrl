%%--------------------------------------------------------------
%% Base 
%%--------------------------------------------------------------
-define(FD_ERR, -1).
-define(CHKID_NULL, 0).
-define(CHK_MAXLEN, 67108864).
-define(CHK_REP, 2).

-define(RW_BUFLEN, 524288).

-define(CALL_TIMEOUT, 30000).

-define(DISK_HB_INTERVAL, 1000).

-type chkop() :: 'add' | 'del'.
-type filename() :: string().
-type cdsname() :: {atom(), node()}.

%%--------------------------------------------------------------
%% common
%%--------------------------------------------------------------
-record(verid,  {id = 0, version = 0}).
-record(diskid, {id = 0, version = 0}).
-record(chkid,  {id = 0, version = 0}).
-record(fileid, {id = 0, version = 0}).

-record(md_file, {
        flen    = 0             :: integer(),
        chklen  = ?CHK_MAXLEN   :: integer(),
        chkrep  = ?CHK_REP      :: integer(),
        chklist = array:new()
    }).

-record(md_chk, {
        chkid  = #chkid{}       :: #chkid{},
        chkoff = 0              :: integer(),
        chklen = 0              :: integer()
    }).

-record(efs_file, {
        path                    :: filename(),
        fid     = #fileid{}     :: #fileid{},
        fmeta   = #md_file{}    :: #md_file{},
        chklist = array:new()
    }).

-record(efs_chk, {
%        chkid  = #chkid{}       :: #chkid{},
%        chkno  = 0              :: integer(),
%        chkrep = 0              :: integer(),
%        chklen = 0              :: integer(),
%        synced = false          :: boolean(),
        loaded = false          :: boolean(),
        disks  = []             :: list()
    }).

%%--------------------------------------------------------------
%% EFS Protocol
%%--------------------------------------------------------------
-record(create,       { path                    } ).
-record(open,         { path                    } ).
-record(close,        { fd                      } ).
-record(pwrite,       { fd, offset, buflen, buf } ).
-record(pread,        { fd, offset, buflen      } ).
-record(fsync,        { fd                      } ).
-record(fstat,        { fd                      } ).
-record(stat,         { path                    } ).
-record(truncate,     { path, len               } ).
-record(fid_truncate, { fid, len                } ).
-record(mkdir,        { path                    } ).
-record(readdir,      { path                    } ).

%%--------------------------------------------------------------
%% MDS protocol
%%--------------------------------------------------------------
-record(mdp_diskjoin_req, {
        name                 :: cdsname(), 
        diskid, 
        diskstat
    }).
-record(mdp_diskjoin_rep, {
        diskid, 
        chkreport
    }).

-record(mdp_diskreport_req, {
        name       :: cdsname(), 
        bin        :: binary()
    }).

-record(mdp_diskhb_req, {
        name                         :: cdsname(), 
        diskid         = #diskid{}   :: #diskid{},
        diskstat_diff  = 0,
        chkreport_list = []
    }).

-record(mdp_create_req, {
        path, 
        mode, 
        fid
    }).
-record(mdp_create_rep, {
        fid   = #fileid{}  :: #fileid{},
        fmeta = #md_file{} :: #md_file{}
    }).

-record(mdp_open_req, {
        path
    }).
-record(mdp_open_rep, {
        fid   = #fileid{}  :: #fileid{},
        fmeta = #md_file{} :: #md_file{}
    }).

-record(mdp_chkget_req, {
        chkid  = #chkid{} :: #chkid{},
        chkno  = 0        :: integer(),
        chkrep = 0        :: integer(),
        chklen = 0        :: integer()
    }).
-record(mdp_chkget_rep, {
        chkid  = #chkid{} :: #chkid{}, 
        chkrep = 0        :: integer(), 
        disks  = []       :: list()
    }).

-record(mdp_chkload_req, {
        chkid  = #chkid{} :: #chkid{},
        chkno  = 0        :: integer(),
        chkrep = 0        :: integer(),
        chklen = 0        :: integer()
    }).
-record(mdp_chkload_rep, {
        chkid  = #chkid{} :: #chkid{}, 
        chkrep = 0        :: integer(), 
        disks  = []       :: list()
    }).

-record(mdp_fsync_req, {
        fid    = #fileid{} :: #fileid{}, 
        chkid  = #chkid{}  :: #chkid{},
        chkno  = 0         :: integer(),
        chkoff = 0         :: integer(),
        chklen = 0         :: integer()
    }).
-record(mdp_fsync_rep, {
        fid    = #fileid{} :: #fileid{},
        chkid  = #chkid{}  :: #chkid{},
        chkno  = 0         :: integer(),
        chkoff = 0         :: integer(),
        chklen = 0         :: integer()
    }).

-record(mdp_getattr_req, {
        path               :: filename()
    }).

-record(mdp_truncate_req, {
        path               :: filename(),
        len                :: integer()
    }).

-record(mdp_fidtruncate_req, {
        fid                :: #fileid{},
        len                :: integer()
    }).

-record(mdp_mkdir_req, {
        path               :: filename()
    }).

%%--------------------------------------------------------------
%% CDS protocol
%%--------------------------------------------------------------
-record(cdp_pwrite_req, {
        chkid  = #chkid{}  :: #chkid{}, 
        chkoff = 0         :: integer(), 
        buflen = 0         :: integer(),
        buf                :: binary()
    }).

-record(cdp_pwrite_rep, {
        chkid, 
        chkoff, 
        count
    }).

-record(cdp_pread_req,  {
        chkid, 
        chkoff, 
        count
    }).
        
%typedef struct {
%        uint32_t ds_bsize;      /* file system block size */
%        uint32_t ds_frsize;     /* fragment size */
%        uint64_t ds_blocks;     /* size of fs in ds_frsize units */ 
%        uint64_t ds_bfree;      /* free blocks */
%        uint64_t ds_bavail;     /* free blocks for non-root */
%        uint64_t ds_files;      /* inodes */
%        uint64_t ds_ffree;      /* free inodes */
%        uint64_t ds_favail;     /* free inodes for non-root */
%        uint32_t ds_fsid;       /* file system ID */
%        uint32_t ds_flag;       /* mount flags */
%        uint32_t ds_namemax;    /* maximum filename length */
%} diskinfo_stat_t;
-record(diskstat, {
        ds_bsize   = 0,
        ds_frsize  = 0,
        ds_blocks  = 0,
        ds_bfree   = 0,
        ds_bavail  = 0,
        ds_files   = 0,
        ds_ffree   = 0,
        ds_favail  = 0,
        ds_fsid    = 0,
        ds_flag    = 0,
        ds_namemax = 0
    }).

%%--------------------------------------------------------------
%% CDS-related
%%--------------------------------------------------------------
%% op = add | del
-record(chkreport, {
        op    = add           :: chkop(),
        chkid = #chkid{}      :: #chkid{}
    }).

-ifdef(debug).
-define(INFOMSG(X), error_logger:info_msg("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(INFOMSG(X), true).
-endif.

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), begin case X of _ -> true end end).
-endif.
