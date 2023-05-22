%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2023 16:19
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("noarkhh").
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/1, stop/0, crash/0]).
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2,
  get_maximum_gradient_stations/0, get_monitor/0]).

start_link(Mode) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Mode, []).

stop() -> gen_server:call(?MODULE, stop).

crash() -> call_function(add_value, []).

add_station(Name, Coordinates) -> call_function(add_station, [Name, Coordinates]).
add_value(Name, Time, Type, Value) -> call_function(add_value, [Name, Time, Type, Value]).
remove_value(Name, Time, Type) -> call_function(remove_value, [Name, Time, Type]).
get_one_value(Name, Time, Type) -> call_function(get_one_value, [Name, Time, Type]).
get_station_mean(Name, Type) -> call_function(get_station_mean, [Name, Type]).
get_daily_mean(Type, Date) -> call_function(get_daily_mean, [Type, Date]).
get_maximum_gradient_stations() -> call_function(get_maximum_gradient_stations, []).

get_monitor() -> gen_server:call(?MODULE, get_monitor).

init(normal) -> {ok, pollution:create_monitor()};
init(test) -> {ok, pollution:create_test_monitor()}.

handle_call({Fun, Args}, _From, Monitor) ->
  NewMonitor = apply(pollution, Fun, Args ++ [Monitor]),
  case NewMonitor of
    {error, _} -> {reply, NewMonitor, Monitor};
    _ -> {reply, NewMonitor, NewMonitor}
  end;
handle_call(get_monitor, _From, Monitor) -> {reply, Monitor, Monitor}.

handle_cast({Fun, Args}, Monitor) -> {noreply, apply(pollution, Fun, Args ++ [Monitor])};
handle_cast(stop, Monitor) -> {stop, stop, Monitor}.

call_function(Fun, Args) -> gen_server:call(?MODULE, {Fun, Args}).

