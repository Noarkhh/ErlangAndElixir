%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Feb 2023 15:58
%%%-------------------------------------------------------------------
-module(my_first_module).
-author("noarkhh").

%% API
-export([mojapierwszafunckja/0, f1/1, factorial/1, power/2, duplicateElements/1, contains/2]).

mojapierwszafunckja () -> 28.

f1(Z) -> Z ++ Z.

factorial(0) -> 1;
factorial(V) -> V * factorial(V - 1).

power(_A, 0) -> 1;
power(A, B) -> A * power(A, B - 1).

duplicateElements([]) -> [];
duplicateElements([L | LS]) -> [L, L | duplicateElements(LS)].

contains([], _Element) -> false;
contains([Element | _Tail], Element) -> true;
contains([_H|T], E) -> contains(T, E).
