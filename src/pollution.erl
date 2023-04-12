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
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3,
  create_test_monitor/0, pipe/2, get_daily_mean/3, mean/1, get_maximum_gradient_stations/1, distance/2,
  calculate_gradient/3]).

-record(monitor, {stations, coordsToNames}).
-record(station, {name, coordinates, measurements}).
-record(measurement, {time, type, value}).

create_monitor() -> #monitor{stations = #{}, coordsToNames = #{}}.

add_station(Name, Coordinates, Monitor) ->
  if
    is_map_key(Name, Monitor#monitor.stations) == true -> {error, "Name already taken"};
    is_map_key(Coordinates, Monitor#monitor.coordsToNames) == true -> {error, "Coordinates already occupied"};
    true ->
      EmptyStation = #station{name = Name, coordinates = Coordinates, measurements = #{}},
      Monitor1 = Monitor#monitor{stations = (Monitor#monitor.stations)#{Name => EmptyStation}},
      Monitor1#monitor{coordsToNames = (Monitor1#monitor.coordsToNames)#{Coordinates => Name}}
  end.

add_value(Name, Time, Type, Value, Monitor) when is_list(Name) ->
  Station = case is_map_key(Name, Monitor#monitor.stations) of
              true -> maps:get(Name, Monitor#monitor.stations);
              false -> error
            end,
  if
    Station == error -> {error, "Station doesn't exist"};
    is_map_key({Time, Type}, Station#station.measurements) == true -> {error, "Measurements already taken"};
    true ->
      NewMeasurement = #measurement{time = Time, type = Type, value = Value},
      UpdatedMeasurements = (Station#station.measurements)#{{Time, Type} => NewMeasurement},
      Monitor#monitor{stations = (Monitor#monitor.stations)#{Name := Station#station{measurements = UpdatedMeasurements}}}
  end;
add_value(Coords, Time, Type, Value, Monitor) when is_tuple(Coords) ->
  case is_map_key(Coords, Monitor#monitor.coordsToNames) of
    true -> add_value(maps:get(Coords, Monitor#monitor.coordsToNames), Time, Type, Value, Monitor);
    false -> {error, "Station doesn't exist"}
  end.


remove_value(Name, Time, Type, Monitor) when is_list(Name) ->
  Station = case is_map_key(Name, Monitor#monitor.stations) of
              true -> maps:get(Name, Monitor#monitor.stations);
              false -> error
            end,
  if
    Station == error -> {error, "Station doesn't exist"};
    is_map_key({Time, Type}, Station#station.measurements) == false -> {error, "No measurements"};
    true ->
      UpdatedMeasurements = maps:remove({Time, Type}, Station#station.measurements),
      Monitor#monitor{stations = (Monitor#monitor.stations)#{Name := Station#station{measurements = UpdatedMeasurements}}}
  end;
remove_value(Coords, Time, Type, Monitor) when is_tuple(Coords) ->
  case is_map_key(Coords, Monitor#monitor.coordsToNames) of
    true -> remove_value(maps:get(Coords, Monitor#monitor.coordsToNames), Time, Type, Monitor);
    false -> {error, "Station doesn't exist"}
  end.

get_one_value(Name, Time, Type, Monitor) when is_list(Name) ->
  Station = case is_map_key(Name, Monitor#monitor.stations) of
              true -> maps:get(Name, Monitor#monitor.stations);
              false -> error
            end,
  if
    Station == error -> {error, "Station doesn't exist"};
    is_map_key({Time, Type}, Station#station.measurements) == false -> {error, "Measurement doesn't exist"};
    true -> (maps:get({Time, Type}, Station#station.measurements))#measurement.value
  end;
get_one_value(Coords, Time, Type, Monitor) when is_tuple(Coords) ->
  case is_map_key(Coords, Monitor#monitor.coordsToNames) of
    true -> get_one_value(maps:get(Coords, Monitor#monitor.coordsToNames), Time, Type, Monitor);
    false -> {error, "Station doesn't exist"}
  end.

get_station_mean(Name, Type, Monitor) when is_list(Name) ->
  Station = case is_map_key(Name, Monitor#monitor.stations) of
              true -> maps:get(Name, Monitor#monitor.stations);
              false -> error
            end,
  if
    Station == error -> {error, "Station doesn't exist"};
    true ->
      IsOfGivenType = fun (_, #measurement{type=T}) when T == Type -> true; (_, _) -> false end,
      FilteredMeasurementsList = maps:values(maps:filter(IsOfGivenType, Station#station.measurements)),
      ValuesList = lists:map(fun (#measurement{value=Value}) -> Value end, FilteredMeasurementsList),
      case mean(ValuesList) of
        nan -> {error, "No measurements of given type"};
        Mean -> Mean
      end
  end;
get_station_mean(Coords, Type, Monitor) when is_tuple(Coords) ->
  case is_map_key(Coords, Monitor#monitor.coordsToNames) of
    true -> get_station_mean(maps:get(Coords, Monitor#monitor.coordsToNames), Type, Monitor);
    false -> {error, "Station doesn't exist"}
  end.

get_daily_mean(Type, Date, Monitor) ->
  GetStationMeasurement =
    fun (#measurement{time={D, _}, type=T, value=V}) when (D == Date) and (T == Type) -> {true, V};
      (_) -> false end,

  GetMonitorMeasurements =
    fun (Station) -> lists:filtermap(GetStationMeasurement, maps:values(Station#station.measurements)) end,

  case
    pipe(Monitor#monitor.stations, [
    {maps, values, []},
    {lists, map, [GetMonitorMeasurements]},
    {lists, append, []},
    {?MODULE, mean, []}
  ]) of
    nan -> {error, "No measurements of given type and date"};
    Mean -> Mean
  end.

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

calculate_gradient(Station1, Station2, Date) ->
  CheckForToday = fun (#measurement{time = {D, _}}) when D == Date -> true; (_) -> false end,
  Measurements1 = group_measurements(lists:filter(CheckForToday, maps:values(Station1#station.measurements))),
  Measurements2 = group_measurements(lists:filter(CheckForToday, maps:values(Station2#station.measurements))),

  CalculateDiff = fun (nan, _) -> 0; (_, nan) -> 0; (V1, V2) -> abs(V1 - V2) end,

  Diffs = lists:zipwith(CalculateDiff, maps:values(Measurements1), maps:values(Measurements2)),

  pipe(Diffs, [
    {lists, map, [fun (X) -> X*X end]},
    {lists, sum, []},
    {math, sqrt, []}
  ]) / distance(Station1#station.coordinates, Station2#station.coordinates).

get_maximum_gradient_stations(Monitor) ->
  StationPairs = [{St1, St2} || St1 <- maps:values(Monitor), St2 <- maps:values(Monitor), St1#station.name > St2#station.name],
  lists:max(lists:map(fun ({St1, St2}) -> {calculate_gradient(St1, St2, erlang:date()), St1, St2} end, StationPairs)).

mean([]) -> nan;
mean(List) -> lists:sum(List) / length(List).

pipe(Data, FunsAndArgs) ->
  lists:foldl(fun({Mod, F, Args}, DataArg) -> apply(Mod, F, Args ++ [DataArg]) end, Data, FunsAndArgs).

create_test_monitor() ->
  pipe(create_monitor(), [
    {pollution, add_station, ["Krakow", {50.057318, 19.926936}]},
    {pollution, add_station, ["Katowice", {50.257229, 19.017763}]},
    {pollution, add_station, ["Warszawa", {52.230614, 21.005862}]},
    {pollution, add_station, ["Lodz", {51.759142, 19.458348}]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{12,48,59}}, "PM2.5", 6919]},
    {pollution, add_value, ["Krakow", {{2023,3,23},{12,48,49}}, "PM2.5", 869]},
    {pollution, add_value, ["Krakow", {{2023,3,27},{14,48,59}}, "PM10", 1029]},
    {pollution, add_value, ["Krakow", {{2023,3,27},{14,48,59}}, "PM2.5", 1573]},
    {pollution, add_value, ["Katowice", {{2023,3,27},{14,48,59}}, "PM2.5", 649]},
    {pollution, add_value, ["Katowice", {{2023,3,27},{14,49,59}}, "PM2.5", 659]},
    {pollution, add_value, ["Warszawa", {{2023,3,27},{14,49,59}}, "PM2.5", 735]},
    {pollution, add_value, ["Warszawa", {{2023,3,27},{14,49,59}}, "PM10", 70035]},
    {pollution, add_value, ["Lodz", {{2023,3,27},{14,49,59}}, "PM10", 700035]}
]).

