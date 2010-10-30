-module(efs_sup).
-behaviour(supervisor).
-export([start/0, start_link/1, init/1]).
-export([get_list_from_env/0, get_list/2]).

-include("efs_test.hrl").
-include("libefs.hrl").

start_link(_Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, _Args).

start() ->
    spawn(fun() -> supervisor:start_link({local,?MODULE}, ?MODULE, []) end).

init(_Args) ->
    %% Install my personal error handler
    %% gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {my_alarm_handler, xyz}),
    Children = get_list(_Args, []),
%    Children = get_list_from_env(),

    ?LOG([Children]),

    {ok, {
            {one_for_one, 3, 10}, 
            Children
        }}.

get_list_from_env() ->
    case application:get_env(efs, mds) of
        undefined ->
            [];
        {ok, _MetaNo} ->
            [get_childspec(mds, I) || I <- lists:seq(1,1)]
    end ++
    case application:get_env(efs, cds) of
        undefined -> 
            [];
        {ok, {M, N}} ->
            [get_childspec(cds, I) || I <- lists:seq(M, N)]
    end ++
    case application:get_env(efs, client) of
        undefined ->
            [];
        _Other ->
            [{tag_efs, {efs, start_link, []}, permanent, 10000, worker, [efs]}]
    end.


get_list([], Acc) -> Acc;
get_list([H|T], Acc) ->
    Acc1 = case H of
        mds ->
            Acc ++ [get_childspec(mds, I) || I <- lists:seq(1, 1)];
        {cds, M, N} ->
            Acc ++ [get_childspec(cds, I) || I <- lists:seq(M, N)];
        client ->
            Acc ++ [{tag_efs, {efs, start_link, []}, permanent, 10000, worker, [efs]}];
        _Other -> 
            Acc
    end,
    get_list(T, Acc1).

get_childspec(mds, MetaNo) ->
    Tag = list_to_atom(atom_to_list(tag_mds) ++ "_" ++ integer_to_list(MetaNo)),
    {Tag,  {efs_mds, start_link, [MetaNo]}, permanent, 10000, worker, [efs_mds]};
get_childspec(cds, MetaNo) ->
    Tag = list_to_atom(atom_to_list(tag_cds) ++ "_" ++ integer_to_list(MetaNo)),
    {Tag, {efs_cds, start_link, [MetaNo]}, permanent, 10000, worker, [efs_cds]}.
