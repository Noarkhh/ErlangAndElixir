%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Mar 2023 15:20
%%%-------------------------------------------------------------------
-module(pingpong).
-author("noarkhh").

%% API
-export([start/0, stop/0, play/1]).
-export([ping/0, pong/0]).

start() ->
  register(ping_proc, spawn(pingpong, ping, [])),
  register(pong_proc, spawn(pingpong, pong, [])).

stop() ->
  ping_proc ! stop,
  pong_proc ! stop.

play(N) ->
  ping_proc ! N.

ping() ->
  receive
    stop -> ok;
    0 -> ping();
    N ->
      io:format("Ping: ~p~n", [N]),
      timer:sleep(300),
      pong_proc ! N - 1,
      ping()
  after
    20000 -> ok
  end.

pong() ->
  receive
    stop -> ok;
    0 -> ping();
    N ->
      io:format("Pong: ~p~n", [N]),
      timer:sleep(300),
      ping_proc ! N - 1,
      pong()
  after
    20000 -> ok
  end.