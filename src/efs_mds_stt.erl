-module(efs_mds_stt).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(mds_stt, {
        path,
        fid,
        type,
        meta
    }).

do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(mds_stt, [
            {attributes, record_info(fields, mds_stt)},
            {index, [fid]},
            {disc_copies, [node()]},
            {type, ordered_set}
        ]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([mds_stt], 20000).

test() ->
    create("/aa", {1,1}, file, {0, 0}),
    create("/cc", {2,1}, file, {0, 0}),
    create("/bb", {3,1}, dir,  {1}),
    fsync({2,1}, 2, {0,0}, 0, 1024),
    ok.

%%--------------------------------------------------------------
%%
%%--------------------------------------------------------------
create(Path, FID, Type, Meta) ->
    Row = #mds_stt{path=Path, fid=FID, type=Type, meta=Meta},
    F = fun() ->
            mnesia:write(Row)
    end,
    mnesia:transaction(F).

fsync(FID, _CHKNO, _CHKID, CHKOFF, CHKLEN) ->
    F = fun() ->
            [R] = mnesia:index_read(mds_stt, FID, #mds_stt.fid),
            R1 = R#mds_stt{meta={CHKOFF, CHKLEN}},
            mnesia:write(R1)
    end,
    mnesia:transaction(F).


%%--------------------------------------------------------------
%%
%%--------------------------------------------------------------
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

demo(select_mds_stt) ->
    do(qlc:q([X || X <- mnesia:table(mds_stt)])).


