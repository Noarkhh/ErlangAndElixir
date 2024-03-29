-module(testproc).
-export([start/0, mul/1, init/0, loop/0]).

start() ->
	register(testP, spawn(testproc, init, [])).

init() ->
	loop().

loop() ->
	receive
	  {Pid, mul, Val} -> 
	    Pid ! {Val * 5, ok},
	    testproc:loop();

	  {Pid, stop} -> 
	    Pid ! {stop, ok}
	end.

mul(Val) -> 
  testP ! {self(), mul, Val},
  receive
    {V, ok} -> V
  end.
