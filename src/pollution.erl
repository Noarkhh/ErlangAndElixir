%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2023 16:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("noarkhh").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, create_test_monitor/0, pipe/2, get_daily_mean/3, mean/1, get_maximum_gradient_stations/1]).

-record(station, {name, coordinates, measurements}).
-record(measurement, {time, type, value}).

create_monitor() -> #{}.

add_station(Monitor, Name, Coordinates) ->
  Monitor#{Name => #station{name = Name, coordinates = Coordinates, measurements = #{}}}.

add_value(Monitor, Name, Time, Type, Value) ->
  Station = maps:get(Name, Monitor),
  NewMeasurement = #measurement{time = Time, type = Type, value = Value},
  UpdatedMeasurements = (Station#station.measurements)#{{Time, Type} => NewMeasurement},
  Monitor#{Name := Station#station{measurements = UpdatedMeasurements}}.

remove_value(Monitor, Name, Time, Type) ->
  Station = maps:get(Name, Monitor),
  UpdatedMeasurements = maps:remove({Time, Type}, Station#station.measurements),
  Monitor#{Name := Station#station{measurements = UpdatedMeasurements}}.

get_one_value(Monitor, Name, Time, Type) ->
  Station = maps:get(Name, Monitor),
  maps:get({Time, Type}, Station#station.measurements).


get_station_mean(Monitor, Name, Type) ->
  Station = maps:get(Name, Monitor),
  IsOfGivenType = fun (_, #measurement{type=T}) when T == Type -> true; (_, _) -> false end,
  FilteredMeasurementsList = maps:values(maps:filter(IsOfGivenType, Station#station.measurements)),
  ValuesList = lists:map(fun (#measurement{value=Value}) -> Value end, FilteredMeasurementsList),
  mean(ValuesList).


get_daily_mean(Monitor, Date, Type) ->
  GetStationMeasurement =
    fun (_, #measurement{time={D, _}, type=T, value=V}) when (D == Date) and (T == Type) -> {true, V};
      (_, _) -> false end,

  GetMonitorMeasurements =
    fun (_K, Station) -> maps:values(maps:filtermap(GetStationMeasurement, Station#station.measurements)) end,

  pipe(maps:map(GetMonitorMeasurements, Monitor), [
    {maps, values, []},
    {lists, append, []},
    {?MODULE, mean, []}
  ]).

%% Dodaj do modułu pollution funkcję get_maximum_gradient_stations, która wyszuka parę stacji, na których wystąpił
%% największy gradient zanieczyszczeń w kontekście odległości.

get_maximum_gradient_stations(Monitor) ->
  StationPairs = [{St1, St2} || {_K1, St1} <- maps:to_list(Monitor), {_K2, St2} <- maps:to_list(Monitor), St1#station.name > St2#station.name].

mean([]) -> 0;
mean(List) -> lists:sum(List) / length(List).

pipe(Data, FunsAndArgs) ->
  lists:foldl(fun({Mod, F, Args}, DataArg) -> apply(Mod, F, [DataArg | Args]) end, Data, FunsAndArgs).

create_test_monitor() ->
  pipe(create_monitor(), [
    {pollution, add_station, ["Krakow", {0, 1}]},
    {pollution, add_station, ["Katowice", {3, 4}]},
    {pollution, add_station, ["Warszawa", {1, 5}]},
    {pollution, add_station, ["Łodz", {5, 4}]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{14,48,59}}, "PM5", 649]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{14,48,49}}, "PM5", 69]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{14,48,59}}, "PM2.5", 29]},
    {pollution, add_value, ["Krakow", {{2023,3,24},{14,48,59}}, "PM2.5", 9]},
    {pollution, add_value, ["Katowice", {{2023,3,23},{14,48,59}}, "PM5", 649]}
]).

