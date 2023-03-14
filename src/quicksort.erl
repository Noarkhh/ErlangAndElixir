%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2023 15:21
%%%-------------------------------------------------------------------
-module(quicksort).
-author("noarkhh").

%% API
-export([qs/1, random_elems/3, compare_speeds/3]).

less_than(List, Arg) -> lists:filter(fun(X) -> X < Arg end, List).
grt_eq_than(List, Arg) -> lists:filter(fun(X) -> X >= Arg end, List).

qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) );
qs([]) -> [].

random_elems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) -> io:format("Fun1: ~w ~n", [element(1, timer:tc(Fun1, [List]))]),
  io:format("Fun2: ~w ~n", [element(1, timer:tc(Fun2, [List]))]).

%%compare_speeds(List, Fun1, Fun2) -> {timer:tc(Fun1, [List]), timer:tc(Fun2, [List])}.
