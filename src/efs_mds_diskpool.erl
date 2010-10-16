-module(efs_mds_diskpool).
-compile(export_all).

-include("libefs.hrl").
-include("efs_mds.hrl").

-record(diskinfo, {
        id   = #diskid {}     :: #diskid{},
        name                  :: cdsname(),
        count = 0             :: integer(),
        stat = #diskstat{}    :: #diskstat{}
    }).

new() ->
    _ = ets:new(mds_diskpool, [set, named_table, protected, {keypos, 2}]).

-spec add(#diskid{}, cdsname(), #diskstat{}) ->
    ok.

add(DiskID, Name, DiskStat) -> 
    case ets:lookup(mds_diskpool, DiskID) of
        [] ->
            ets:insert(mds_diskpool, #diskinfo{id=DiskID, name=Name, stat=DiskStat}),
            ok;
        [_] ->
            ok
    end.

-spec chkget(#chkid{}, integer()) -> list().

chkget(_ChkID, ChkRep) -> 
    L = ets:tab2list(mds_diskpool),
    L1 = lists:sublist(lists:keysort(4, L), ChkRep),
    lists:foreach(fun update_count/1, L1),
    [Name || #diskinfo{name=Name} <- L1].

update_count(#diskinfo{count=Count} = DI) ->
    ets:insert(mds_diskpool, DI#diskinfo{count=Count+1}).


