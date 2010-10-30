-module(efs_tests).

-include_lib("eunit/include/eunit.hrl").

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
