-module(libefs).
-export([
        id_to_list/1,
        is_id/1,
        time/0,
        unconsult/2,
        pwrite/3,
        pread/3,
        sleep/1,
        dump/2,
        home_path/2,
        vec_cal/2,
        vec_limit/3,
        vec_update/4
    ]).

%-compile(export_all).

-include("efs_test.hrl").
-include("libefs.hrl").

%id_to_list(ID, Version) ->
%    integer_to_list(ID) ++ "_v" ++ integer_to_list(Version).

%id_to_list(#chkid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version);
%id_to_list(#fileid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version);
%id_to_list(#diskid_t{id=ID, version=Version}) ->
%    id_to_list(ID, Version).

id_to_list({_, ID, Version}) ->
    integer_to_list(ID) ++ "_v" ++ integer_to_list(Version).

is_id({_, ID, Version}) ->
    if 
        ID =:= 0 orelse Version =:= 0 ->
            false;
        true ->
            true
    end.

%%--------------------------------------------------------------
%% File IO
%%--------------------------------------------------------------
unconsult(File, L) ->
    {ok, S} = file:open(File, [write]),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).

pwrite(File, Off, Buf) ->
    {ok, S} = file:open(File, [read,write,raw,binary]),
    ok = file:pwrite(S, Off, Buf),
    file:close(S).

-spec pread(string(), integer(), integer()) -> {ok, binary()} | {error, eof}.

pread(File, Off, Count) ->
    {ok, S} = file:open(File, [read,binary,raw]),
    case file:pread(S, Off, Count) of
        {ok, Buf} ->
            file:close(S),
            {ok, Buf};
        _Other ->
            file:close(S),
            {error, eof}
    end.


%%--------------------------------------------------------------
%% utils
%%--------------------------------------------------------------
time() -> 
    Now = now(),
    element(1,Now)*1000000+element(2,Now).

sleep(T) ->
    receive 
    after T -> ok
    end.

-define(NYI(X), 
    (begin
        io:format("*** NYI ~p ~p ~p~n", [?MODULE, ?LINE, X]),
        exit(nyi)
    end)).

dump(File, Term) ->
    Out = File ++ ".tmp",
    io:format("** dumping to ~s~n", [Out]),
    {ok, S} = file:open(Out, [append]),
    io:format(S, "~p.~n", [Term]),
    file:close(S).

%%--------------------------------------------------------------
%% configure
%%--------------------------------------------------------------
home_path(cds, MetaNo) ->
    "/home/gj/mylove/git/efs/efsroot/cds/" ++ integer_to_list(MetaNo) ++ "/"; 
home_path(mds, MetaNo) ->
    "/home/gj/mylove/git/efs/efsroot/mds/" ++ integer_to_list(MetaNo) ++ "/". 

%%--------------------------------------------------------------
%% vector
%%--------------------------------------------------------------
vec_cal(Offset, MaxLen) ->
    {Offset div MaxLen, Offset rem MaxLen}.

vec_limit(Offset, Len, MaxLen) ->
    if 
        Offset >= MaxLen -> 0;
        Offset + Len >= MaxLen -> MaxLen - Offset;
        true -> Len
    end.

vec_update(NewOff, NewLen, OldOff, OldLen) ->
    NewEnd = NewOff + NewLen,
    OldEnd = OldOff + OldLen,

    ResOff = if 
        NewOff < OldOff -> NewOff;
        true            -> OldOff
    end,
    ResEnd = if
        NewEnd > OldEnd -> NewEnd;
        true            -> OldEnd
    end,
    {ResOff, ResEnd - ResOff}.

%%--------------------------------------------------------------
-ifdef(TEST).
simple_test() ->
    ?assert(libefs:id_to_list({0, 0, 0}) =:= "0_v0"),
    ?assert(libefs:id_to_list({0, 1, 2}) =:= "1_v2"),

    ?assertEqual(libefs:is_id({0,0,0}), false),
    ?assertEqual(libefs:is_id({0,0,1}), false),
    ?assertEqual(libefs:is_id({0,1,0}), false),
    ?assertEqual(libefs:is_id({0,1,1}), true),

    false = libefs:is_id({0,0,0}),
    false = libefs:is_id({0,0,1}),
    false = libefs:is_id({0,1,0}),
    true  = libefs:is_id({0,1,1}),

    ?assert(libefs:home_path(cds, 1) =:= "/home/gj/mylove/git/efs/efsroot/cds/1/"),
    ?assert(libefs:home_path(cds, 2) =:= "/home/gj/mylove/git/efs/efsroot/cds/2/"),
    ?assert(libefs:home_path(mds, 1) =:= "/home/gj/mylove/git/efs/efsroot/mds/1/"),
    ?assert(libefs:home_path(mds, 2) =:= "/home/gj/mylove/git/efs/efsroot/mds/2/"),

    ?assert(libefs:vec_cal(5,4) =:= {1,1}),
    ?assert(libefs:vec_cal(6,4) =:= {1,2}),

    ?assert(libefs:vec_limit(5,1,8) =:= 1),
    ?assert(libefs:vec_limit(5,10,8) =:= 3),
    ?assert(libefs:vec_limit(8,10,8) =:= 0),

    ok.

generator_test_() -> [
    % representing a test as data
    fun() -> ?assert(1 =:= 1) end,

    % using macros to write tests
    ?_test(?assert(libefs:id_to_list({0, 0, 0}) =:= "0_v0")),
    ?_test(?assert(libefs:id_to_list({0, 1, 2}) =:= "1_v2")),

    % underscore-prefixed macros create test objects
    ?_assert(libefs:id_to_list({0, 0, 0}) =:= "0_v0"),
    ?_assert(libefs:id_to_list({0, 1, 2}) =:= "1_v2"),

    % test set with title
    {"vec test", [
        ?_assert(libefs:vec_cal(5,4) =:= {1,1}),
        ?_assert(libefs:vec_cal(6,4) =:= {1,2}),

        ?_assert(libefs:vec_limit(5,1,8) =:= 1),
        ?_assert(libefs:vec_limit(5,10,8) =:= 3),
        ?_assert(libefs:vec_limit(8,10,8) =:= 0)
    ]},

    ?_assertNot(false),
    ?_assertEqual(false, false),

    ?_assertException(throw, {not_found, _}, throw({not_found, 42})),
    ?_assertThrow({not_found, _}, throw({not_found, 42})),
    ?_assertError(badarith, 1/0),
    ?_assertExit(normal, exit(normal)),

    ?_assertCmd("ls"),
    ?_assertCmdStatus(0, "ls"),
    ?_cmd("ls"),

    % test fixture
    {"file setup and cleanup", 
        setup,
        fun() -> ?cmd("mktemp tmp.XXXXXXXX") end,
        fun(File) -> 
                %% debug macros
                ?debugMsg(File),
                ?cmd("rm " ++ File) 
        end,
        fun(File) -> [
                ?_assertCmd("echo xyzzy >" ++ File),
                ?_assertCmdOutput("xyzzy\n", "cat " ++ File)
             ]
        end
    },

    ?_assert(1 =:= 1)
    ].
-endif.
