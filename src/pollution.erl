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
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4]).

-record(station, {name, coordinates, measurements}).
-record(measurement, {time, type, value}).

create_monitor() -> #{}.

add_station(Monitor, Name, Coordinates) -> Monitor#{Name => #station{name = Name, coordinates = Coordinates, measurements = #{}}}.

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
