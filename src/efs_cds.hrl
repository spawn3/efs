-define(DIR_DISK,  "disk/").
-define(DIR_JNL,   "disk/jnl/").
-define(DIR_CHUNK, "chunk/").

-define(EFS_CDS_CHK_OFF, 0).

%% $CDS_ROOT$/chunk/
-record(md_chk_head, {
        crcode  = 0,
        cds_ver = 1,
        time    = 0,
        fid     = #fileid{}   :: #fileid{},
        chkid   = #chkid{}    :: #chkid{},
        chkno   = 0,
        chkoff  = 0,
        chklen  = 0
    }).


%% $CDS_ROOT$/disk/jnl/
-record(md_chk_jnl, {
        magic   = 16#1        :: integer(),
        time    = 0           :: integer(),
        op      = add         :: chkop(),
        chkid   = #chkid{}    :: #chkid{},
        version = 0           :: integer()
    }).

