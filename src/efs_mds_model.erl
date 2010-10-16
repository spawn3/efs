-module(efs_mds_model).
-export([
        getattr/2,
        truncate/3,
        fsync/6
    ]).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("libefs.hrl").
-include("efs_mds.hrl").

%-record(file_info, {
%        fid  = #fileid{}  :: #fileid{},
%        fmeta = #md_file{} :: #md_file{}
%    }).


get_idfile(Home, FID) ->
    filename:join([Home, ?DIR_FILEID, libefs:id_to_list(FID)]).

get_fid(Home, Path) ->
    File = filename:join([Home, ?DIR_ROOT, Path]),
    case filelib:is_regular(File) of
        false ->
            {error, nofile};
        true ->
            case file:consult(File) of
                {ok, [#fileid{} = FID]} ->
                    {ok, FID};
                _ ->
                    {error, nofile}
            end
    end.

-spec open(filename(), filename()) -> 
    {ok, #mdp_open_rep{}} |
    {error, atom()}.

open(Home, Path) ->
    case get_fid(Home, Path) of
        {ok, FID} ->
            IdFile = get_idfile(Home, FID),
            case file:consult(IdFile) of
                {ok, [#md_file{} = Fmeta]} ->
                    {ok, #mdp_open_rep{fid=FID, fmeta=Fmeta}};
                _ ->
                    {error, nofile}
            end;
        {error, _Reason} -> 
            {error, _Reason}
    end.

-spec fsync(filename(), #fileid{}, #chkid{}, integer(), integer(), integer()) ->
    ok.

fsync(Home, FID, ChkID, ChkNo, NewChkOff, NewChkLen) ->
    IdFile = get_idfile(Home, FID),
    case file:consult(IdFile) of
        {ok, [#md_file{chklist=CL} = Fmeta]} ->
            CL1 = case array:get(ChkNo, CL) of
                undefined ->
                    array:set(ChkNo, #md_chk{chkid=ChkID, 
                            chkoff=NewChkOff, 
                            chklen=NewChkLen}, CL);
                #md_chk{chkoff=OldChkOff, chklen=OldChkLen} = Chk ->
                    ?assertEqual(ChkID, Chk#md_chk.chkid),
                    ChkRange = libefs:vec_update(NewChkOff, NewChkLen, 
                        OldChkOff, OldChkLen),
                    array:set(ChkNo, Chk#md_chk{
                            chkoff=element(1, ChkRange), 
                            chklen=element(2, ChkRange)}, CL)
            end,

            ChkNum = array:size(CL1),
            Flen = if 
                %% last chk
                ChkNo + 1 =:= ChkNum ->
                    #md_chk{chkoff=ChkOff, chklen=ChkLen} = array:get(ChkNo, CL1),
                    ChkNo * Fmeta#md_file.chklen + ChkOff + ChkLen;
                true -> 
                    Fmeta#md_file.flen
            end,

            _ = libefs:unconsult(IdFile, [Fmeta#md_file{flen=Flen, chklist=CL1}]),
            ok;
        _Other -> 
            ok
    end,
    ok.

getattr(Home, #fileid{} = FID) ->
    IdFile = get_idfile(Home, FID),
    case file:consult(IdFile) of
        {ok, [#md_file{} = Fmeta]} ->
            {FID, Fmeta};
        _Other ->
            {error, nofile}
    end;
getattr(Home, Path) ->
    case get_fid(Home, Path) of
        {ok, FID} ->
            getattr(Home, FID);
        {error, _Reason} ->
            {error, _Reason}
    end.

-spec truncate(filename(), filename(), integer()) -> ok | {error, nofile}
             ;(filename(), #fileid{}, integer()) -> ok | {error, nofile}.

truncate(Home, #fileid{} = FID, Length) ->
    IdFile = get_idfile(Home, FID),
    case file:consult(IdFile) of
        {ok, [#md_file{flen=Flen, chklist=CL} = Fmeta]} ->
            if 
                Length >= Flen -> 
                    ok;
                true ->
                    {ChkNo, ChkOff} = libefs:vec_cal(Length, Fmeta#md_file.chklen),

                    CL1 = array:resize(ChkNo + 1, CL),
                    case array:get(ChkNo, CL1) of
                        #md_chk{chkoff=OldChkOff, chklen=OldChkLen} = Chk ->
                            {NewChkOff, NewChkLen} = 
                            if 
                                ChkOff =< OldChkOff ->
                                    {0, 0};
                                ChkOff =< OldChkOff + OldChkLen ->
                                    {OldChkOff, ChkOff - OldChkOff};
                                true ->
                                    {OldChkOff, OldChkLen}
                            end,
                            CL2 = array:set(ChkNo, Chk#md_chk{chkoff=NewChkOff, chklen=NewChkLen}, CL1),
                            _ = libefs:unconsult(IdFile, [Fmeta#md_file{flen=Length, chklist=CL2}]),
                            ok;
                        undefined ->
                            {error, nofile}
                    end
            end;
        _Other ->
            {error, nofile}
    end;
truncate(Home, Path, Length) ->
    case get_fid(Home, Path) of
        {ok, FID} ->
            truncate(Home, FID, Length);
        {error, _Reason} ->
            {error, _Reason}

    end.

-spec mkdir(string(), string()) -> ok | {error, atom()}.

mkdir(Home, Path) ->
    Dir = filename:join([Home, ?DIR_ROOT, Path]),
    ?LOG("mkdir " ++ Dir),
    case filelib:is_dir(Dir) of
        false ->
            filelib:ensure_dir(Dir ++ "/");
        true ->
            {error, exist}
    end.

-spec readdir(filename(), filename()) -> {ok, list()} | {error, atom()}.

readdir(Home, Path) ->
%    Dir = filename:join([Home, ?DIR_ROOT, Path]),
    Dir = Home ++ ?DIR_ROOT ++ Path,
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, Filenames};
        {error, _Reason} ->
            {error, _Reason}
    end.
