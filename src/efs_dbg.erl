-module(efs_dbg).
-compile(export_all).

-include_lib("stdlib/include/ms_transform.hrl").

start() ->
    dbg:tracer(),
    dbg:p(all, [c]),
    efs_dbg:tpl(efs,chkget),
    efs_dbg:tpl(efs_mds,create),
    efs_dbg:tpl(efs_mds_model,fsync),
    efs_dbg:tpl(efs_mds_diskpool,chkget),
    efs_dbg:tpl(efs_mds_chkpool,chkload),
    ok.

tpl(M, F, A) ->
    Ms = dbg:fun2ms(fun(_) -> return_trace() end),
    dbg:tpl(M, F, A, Ms).

tpl(M, F) ->
    tpl(M, F, '_').

tpl(M) ->
    tpl(M, '_', '_').
