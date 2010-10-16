-module(efs_mds_chkpool).
-compile(export_all).

-include("libefs.hrl").
-include("efs_mds.hrl").

-record(chkinfo, {
        chkid = #chkid{}      :: #chkid{},
        disks = []            :: list()
    }).

new() ->
    _ = ets:new(mds_chkpool,  [set, named_table, protected, {keypos, 2}]).

report(DiskID, L) ->
    F = fun (#chkreport{op=OP, chkid=ChkID}) ->
            case OP of
                add -> add(ChkID, DiskID);
                del -> del(ChkID, DiskID)
            end
    end,
    lists:foreach(F, L).

add(ChkID, DiskID) ->
    case ets:lookup(mds_chkpool, ChkID) of
    [] -> 
        ets:insert(mds_chkpool, #chkinfo{chkid=ChkID, disks=[DiskID]});
    [#chkinfo{disks=Disks} = CI] ->
        case lists:member(DiskID, Disks) of
            false ->
                ets:insert(mds_chkpool, CI#chkinfo{disks=[DiskID|Disks]});
            true -> 
                true
        end
    end.

del(ChkID, DiskID) ->
    case ets:lookup(mds_chkpool, ChkID) of
        [] -> true;
        [#chkinfo{disks=Disks} = CI] -> 
            case lists:member(DiskID, Disks) of
                false -> 
                    true;
                true ->
                    ets:insert(mds_chkpool, CI#chkinfo{disks=Disks -- [DiskID]})
            end
    end.

-spec chkload(#chkid{}) -> list().

chkload(ChkID) ->
    case ets:lookup(mds_chkpool, ChkID) of
        [] -> 
            [];
        [#chkinfo{disks=Disks}] ->
            Disks
    end.

