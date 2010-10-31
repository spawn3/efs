#!/usr/bin/env escript

-record(r_opt, {
        t = slave :: 'master' | 'slave',
        n = 1     :: integer(),
        m = []    :: [string()]
    }).

main(Args) ->
    Opt = get_opt(Args, #r_opt{}),
    io:format("~p~p~n", [Args, Opt]),

    %% @todo
    DefaultM = ["192.168.1.101", "192.168.1.102"],
    Opt1 = case Opt#r_opt.m of
        [] -> Opt#r_opt{m=DefaultM};
        _ -> Opt
    end,
    io:format("~s~n", [out(Opt1)]),
    ok.

get_opt([], #r_opt{m=Masters}=Opt) -> 
    Opt#r_opt{m=lists:reverse(Masters)};
get_opt([H|T], #r_opt{m=Masters} = Opt) ->
    case H of
        "-m" ->
            get_opt(T, Opt#r_opt{t=master});
        "-n" ->
            [H1|T1] = T,
            get_opt(T1, Opt#r_opt{n=list_to_integer(H1)});
        Master when length(Master) > 8 ->
            get_opt(T, Opt#r_opt{m=[Master|Masters]})
    end.

out(#r_opt{t=master, n=N, m=Masters}) ->
    io_lib:format(
        "[\n"
        "~s\n"
        "~s\n"
        "~s\n"
        "~s\n"
        "].\n", [
            gen_kernel(Masters, N),
            gen_inets(),
            gen_sasl(),
            gen_edog(Masters)
        ]);
out(#r_opt{t=slave, m=Masters}) ->
    io_lib:format(
        "[\n"
        "~s\n"
        "~s\n"
        "].\n", [
            gen_sasl(),
            gen_edog(Masters)
        ]).

gen_kernel(Masters, N) ->
    io_lib:format(
    "{kernel, [\n"
    "\t{distributed, [\n~s\n\t]},\n"
    "\t{optional, [\n~s\n\t]}\n"
    "]}]},", [
        gen(Masters, ""),
        gen(lists_sub(Masters, N), "")
    ]).

gen_sasl() ->
    "{sasl, []},".

gen_inets() ->
    "{inets, []},".

gen_edog(Masters) ->
    io_lib:format(
    "{edog, [\n"
    "\t{edog_masters, [\n~s\n\t]}\n"
    "]}", [gen(Masters, "")]).

gen([], Acc) -> Acc;
gen([H|T], Acc) ->
    NewAcc = case T of
        [] -> io_lib:format("~s\t\t'edog_master@~s'",    [Acc, H]);
        _  -> io_lib:format("~s\t\t'edog_master@~s',\n", [Acc, H])
    end,
    gen(T, NewAcc).

lists_sub(L, N) when N > 0 andalso N =< length(L) ->
    lists:delete(lists:nth(N, L), L).
