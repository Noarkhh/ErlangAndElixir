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
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, create_test_monitor/0, pipe/2, get_daily_mean/3, mean/1, get_maximum_gradient_stations/1, distance/2, calculate_gradient/3, calculate_diff/2]).

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

distance({Lat1, Lon1}, {Lat2, Lon2}) ->
  R = 6371000,
  Phi1 = Lat1 * math:pi() / 180,
  Phi2 = Lat2 * math:pi() / 180,
  DPhi = (Lat1 - Lat2) * math:pi() / 180,
  DLambda = (Lon1 - Lon2) * math:pi() / 180,
  A = math:sin(DPhi / 2) * math:sin(DPhi / 2) +
      math:cos(Phi1) * math:cos(Phi2) * math:sin(DLambda / 2) * math:sin(DLambda / 2),
  C = 2 * math:atan2(math:sqrt(A), math:sqrt(1 - A)),
  R * C.

group_measurements(L) ->
  ParamMap = #{"PM2.5" => [], "PM10" => [], "O3" => [], "NO2" => [], "SO2" => [], "CO" => []},
  Folded = lists:foldl(fun (#measurement{type=T, value=V}, Map) -> Map#{T => [V | maps:get(T, Map)]} end, ParamMap, L),
  maps:map(fun (_K, V) -> mean(V) end, Folded).

calculate_diff(nan, _) -> 0;
calculate_diff(_, nan) -> 0;
calculate_diff(V1, V2) -> abs(V1 - V2).


calculate_gradient(Station1, Station2, Date) ->
%%  Types = ["PM2.5", "PM10", "O3", "NO2", "SO2", "CO"],
  CheckForToday = fun (#measurement{time = {D, _}}) when D == Date -> true; (_) -> false end,
  Measurements1 = group_measurements(lists:filter(CheckForToday, maps:values(Station1#station.measurements))),
  Measurements2 = group_measurements(lists:filter(CheckForToday, maps:values(Station2#station.measurements))),
  Diffs = lists:zipwith(calculate_diff, maps:values(Measurements1), maps:values(Measurements2)),
  math:sqrt(lists:sum(lists:map(fun (X) -> X*X end, Diffs))) /
    distance(Station1#station.coordinates, Station2#station.coordinates).

get_maximum_gradient_stations(Monitor) ->
  StationPairs = [{St1, St2} || St1 <- maps:values(Monitor), St2 <- maps:values(Monitor), St1#station.name > St2#station.name],
  GetMaxStations = fun({St11, St12, V1}, {_, _, V2}) when V1 > V2 -> {St11, St12, V1}; (_, {St21, St22, V2}) -> {St21, St22, V2} end,
  lists:foldl(GetMaxStations, {0, 0, -infinity}, lists:map(fun ({St1, St2}) -> {St1, St2, calculate_gradient(St1, St2, erlang:date())} end, StationPairs)).

mean([]) -> nan;
mean(List) -> lists:sum(List) / length(List).

pipe(Data, FunsAndArgs) ->
  lists:foldl(fun({Mod, F, Args}, DataArg) -> apply(Mod, F, [DataArg | Args]) end, Data, FunsAndArgs).

create_test_monitor() ->
  pipe(create_monitor(), [
    {pollution, add_station, ["Krakow", {0, 1}]},
    {pollution, add_station, ["Katowice", {3, 4}]},
    {pollution, add_station, ["Warszawa", {1, 5}]},
    {pollution, add_station, ["Łodz", {5, 4}]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{14,48,59}}, "PM2.5", 649]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{14,48,49}}, "PM2.5", 69]},
    {pollution, add_value, ["Krakow", {{2023,3,27},{14,48,59}}, "PM10", 29]},
    {pollution, add_value, ["Katowice", {{2023,3,27},{14,48,59}}, "PM2.5", 649]},
    {pollution, add_value, ["Katowice", {{2023,3,27},{14,49,59}}, "PM2.5", 659]}
]).

