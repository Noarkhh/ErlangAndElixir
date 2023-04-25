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
-export([init/1, handle_call/3, handle_cast/2, start_link/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
  {ok, pollution:create_monitor()}.

handle_call({Fun, Args}, _From, Monitor) -> {reply, apply(pollution, Fun, Args ++ [Monitor])}.

handle_cast({Fun, Args}, Monitor) -> {noreply, apply(pollution, Fun, Args ++ [Monitor])}.

add_station(Name, Coordinates) -> gen_server:call(?MODULE, {add_station, [Name, Coordinates]}).
add_value(Name, Time, Type, Value) -> call(add_value, [Name, Time, Type, Value]).
remove_value(Name, Time, Type) -> call(remove_value, [Name, Time, Type]).
get_one_value(Name, Time, Type) -> call(get_one_value, [Name, Time, Type]).
get_station_mean(Name, Type) -> call(get_station_mean, [Name, Type]).
get_daily_mean(Type, Date) -> call(get_daily_mean, [Type, Date]).
get_maximum_gradient_stations() -> call(get_maximum_gradient_stations, []).

