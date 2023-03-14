%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2023 02:44
%%%-------------------------------------------------------------------
-module(my_first_module).
-author("noarkhh").

%% API
-export([ojapierwszafunckia/0, contains/2, duplicateElements/1, power/2, factorial/1, f1/1, rpn/2, rpn_string/1]).

ojapierwszafunckia() -> 28.

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

rpn_string(Str) -> rpn(string:lexemes(Str, " "), []).

rpn(["+" | Tail], [A, B | T]) -> rpn(Tail, [B + A | T]);
rpn(["-" | Tail], [A, B | T]) -> rpn(Tail, [B - A | T]);
rpn(["*" | Tail], [A, B | T]) -> rpn(Tail, [B * A | T]);
rpn(["/" | Tail], [A, B | T]) -> rpn(Tail, [B / A | T]);
rpn(["sqrt" | Tail], [A | T]) -> rpn(Tail, [math:sqrt(A) | T]);
rpn(["sin" | Tail], [A | T]) -> rpn(Tail, [math:sin(A) | T]);
rpn(["cos" | Tail], [A | T]) -> rpn(Tail, [math:cos(A) | T]);
rpn(["tan" | Tail], [A | T]) -> rpn(Tail, [math:tan(A) | T]);
rpn(["pow" | Tail], [A, B | T]) -> rpn(Tail, [math:pow(B, A) | T]);
rpn([Num | Tail], Stack) -> rpn(Tail, [element(1, string:to_float(Num)) | Stack]);
rpn([], [Res | _T]) -> Res.


