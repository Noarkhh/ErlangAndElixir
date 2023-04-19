%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2023 01:37
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("noarkhh").

%% API
-export([start/0, init/1, add_station/2, stop/0, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
     get_daily_mean/2, get_maximum_gradient_stations/0, start_test/0, get_monitor/0]).

start() ->
     register(pollution_monitor, spawn(?MODULE, init, [main])).

start_test() ->
     register(pollution_monitor, spawn(?MODULE, init, [test])).

init(main) -> loop(pollution:create_monitor());
init(test) -> loop(pollution:create_test_monitor()).

loop(Monitor) ->
     receive
          {Pid, FunctionName, Args} ->
               M = apply(pollution, FunctionName, Args ++ [Monitor]),
               Pid ! M,
               loop(M);
          {Pid, get} ->
               Pid ! Monitor,
               loop(Monitor);
          {Pid, stop} -> Pid ! stop
     end.

stop() ->
     pollution_monitor ! {self(), stop},
     receive
          S -> S
     end.

call(FunctionName, Args) ->
     pollution_monitor ! {self(), FunctionName, Args},
     receive
          M -> M
     end.

get_monitor() ->
     pollution_monitor ! {self(), get},
     receive
          M -> M
     end.

add_station(Name, Coordinates) -> call(add_station, [Name, Coordinates]).
add_value(Name, Time, Type, Value) -> call(add_value, [Name, Time, Type, Value]).
remove_value(Name, Time, Type) -> call(remove_value, [Name, Time, Type]).
get_one_value(Name, Time, Type) -> call(get_one_value, [Name, Time, Type]).
get_station_mean(Name, Type) -> call(get_station_mean, [Name, Type]).
get_daily_mean(Type, Date) -> call(get_daily_mean, [Type, Date]).
get_maximum_gradient_stations() -> call(get_maximum_gradient_stations, []).

