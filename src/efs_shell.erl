%% @author gj <happydgj@gmail.com>
%% @doc The shell of EFS.
%% @reference <a href="http://www.erlang.org">Erlang World</a>
%% @copyright 2010
%%
%% @todo add C API
%% @todo add Python API
%% @todo add NFS support

-module(efs_shell).
-export([
        get_bin/2,
        put_file/1,
        put_file/2,
        get_file/1,
        test_write/1,
        test_write/4,
        ww/2,
        rr/1,
        rr/0,
        ss/0
    ]).
%-compile(export_all).

-include_lib("stdlib/include/ms_transform.hrl").
-include("efs_test.hrl").
-include("libefs.hrl").

%%--------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------
get_bin(0, B) -> B;
get_bin(N, B) ->
    B1 = <<B/binary, B/binary>>,
    get_bin(N-1, B1).

put_file(Efd, S, Off, Count) ->
    case file:pread(S, Off, Count) of
        {ok, Buf} -> 
            Buflen = byte_size(Buf),
            efs:pwrite(Efd, Off, Buflen, Buf),
            put_file(Efd, S, Off+Buflen, Count);
        _Other ->
            file:close(S),
            {error, eof}
    end.

put_file1(_, _, 0) -> ok;
put_file1(Efd, Off, Size) ->
    B = get_bin(14, <<"text">>), %% 512K
    Buflen = erlang:min(Size, byte_size(B)),
    <<B1:Buflen/binary, _/binary>> = B,
    efs:pwrite(Efd, Off, Buflen, B1),
    put_file1(Efd, Off+Buflen, Size-Buflen).

put_file(N, Size) when is_integer(N) ->
    statistics(runtime),
    statistics(wall_clock),
    V = libefs:time(),
    Path = integer_to_list(N) ++ "_v" ++ integer_to_list(V),
    case efs:create(Path) of
        {ok, Efd} ->
            put_file1(Efd, 0, Size),
            ok;
        {error, _Reason} ->
            ok
    end,

%    {_, T1} = statistics(runtime),
%    {_, T2} = statistics(wall_clock),
%    io:format("~p: ~p ~p~n", [N, T1, T2]),
    ok.

put_file(Path) ->
    case file:open(Path, [read, binary]) of
        {ok, S} ->
            Base = filename:basename(Path),
            case efs:create(Base) of
                {ok, Efd} ->
                    put_file(Efd, S, 0, ?RW_BUFLEN);
                {error, _Reason} ->
                    {error, _Reason}
            end;
        {error, _Reason} ->
            {error, _Reason}
    end.

get_file(Path) ->
    {ok, S} = file:open(Path, [write,binary]),
    case efs:open(Path) of 
        {ok, Efd} ->
            get_file(Efd, 0, ?RW_BUFLEN, S),
            efs:close(Efd);
        _Other ->
            ?LOG([_Other])
    end,
    file:close(S),
    ok.

get_file(Fd, Offset, Count, S) ->
    case efs:pread(Fd, Offset, Count) of
        {ok, Buf} ->
            file:pwrite(S, Offset, Buf),
            get_file(Fd, Offset+byte_size(Buf), Count, S);
        {error, _Reason} ->
            {error, _Reason}
    end.

spawn_join(0) -> ok;
spawn_join(N) ->
    receive
        _Any ->
            ?LOG([N, _Any]),
            spawn_join(N-1)
    end.

%% @doc write N files to EFS and each file size is Size bytes.
%% @spec ww(integer(), integer()) -> any()

ww(N, Size) ->
    process_flag(trap_exit, true),
    lists:foreach(fun(I) -> spawn_link(?MODULE, put_file, [I, Size]) end, lists:seq(1, N)),
    spawn_join(N).

rr() ->
    {ok, Files} = efs:readdir("/"),
    lists:foreach(fun get_file/1, Files).

traverse_multi1(_L, I, N, _Step) when I > N -> ok;
traverse_multi1(L, I, N, Step) ->
    L1 = lists:sublist(L, I, Step),
    process_flag(trap_exit, true),
    lists:foreach(fun(X) -> spawn_link(?MODULE, get_file, [X]) end, L1),
    spawn_join(length(L1)),
    traverse_multi1(L, I+length(L1), N, Step).

rr(Step) ->
    {ok, Files} = efs:readdir("/"),
    N = length(Files),
    traverse_multi1(Files, 1, N, Step).

ss() ->
    {ok, Files} = efs:readdir("/"),
    lists:foreach(fun ss1/1, Files).

ss1(Path) ->
    ?LOG([Path, efs:stat(Path)]).

%%--------------------------------------------------------------
%% tests
%%--------------------------------------------------------------
test_write(0, _, _, _) -> ok;
test_write(N, Efd, Offset, Bin) ->
    ?LOG([N, Offset]),
    Size = byte_size(Bin),
    efs:pwrite(Efd, Offset, Size, Bin),
    test_write(N-1, Efd, Offset+Size, Bin).

test_write(Path) ->
%    Bin = get_bin(1, <<"text">>),
    Efd = efs:create(Path),
    case Efd of
        ?FD_ERR ->
            ok;
        _Other ->
            test_write(11, Efd, 0, <<"text">>),
            efs:show(),
            efs:close(Efd),
            efs:show(),
            efs:stat(Path)
    end.

-ifdef(TEST).
efs_test_() -> 
    [
        fun() ->
            A = array:new(),
            A1 = array:set(10, 1, A),

            ?assertEqual(array:is_fix(A), false),
            ?assertEqual(array:size(A), 0),
            ?assertEqual(array:is_fix(A1), false),
            ?assertEqual(array:size(A1), 11),
            ?assertEqual(array:get(9, A1), undefined),
            ?assertEqual(array:get(10, A1), 1)
        end,

        {"write and read",
            setup,
            fun() -> 
                    application:start(sasl),
                    application:start(efs),
                    ww(10,1024*1024),
                    timer:sleep(3000),
                    ?debugMsg("rr"),
                    rr(),
                    ?debugMsg("rr step"),
                    rr(10)
            end,
            fun(_) -> ok end,
            [
                ?_assert(1 =:= 1)
            ]
        },

%        fun() -> test_write("aa") end,

        ?_assert(true)
    ].

lazy_test_() ->
    {spawn, [
            ?_test(undefined = put(count, 0)), 
            lazy_gen(7), 
            ?_assertMatch(7, get(count))]}.

lazy_gen(N) ->
    {generator,
        fun () ->
                if N > 0 ->
                        [?_test(put(count,1+get(count))) | lazy_gen(N-1)];
                    true -> []
                end
        end}.

-endif.
