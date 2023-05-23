%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2023 20:56
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("noarkhh").
-behavior(gen_statem).

%% API
-export([init/1, callback_mode/0, start_link/0, stop/0, set/3, add/3]).
-export([store_data/0, add_value/3, set_station/1]).

-record(station_measurements, {station, measurements}).
-record(measurement, {time, type, value}).

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_statem:stop(?MODULE).

set_station(Station) -> gen_statem:cast(?MODULE, {setting, Station}).
add_value(Time, Type, Value) -> gen_statem:cast(?MODULE, {adding, Time, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, storing).

init([]) ->
  {ok, set, []}.
callback_mode() -> state_functions.

set(_Event, {setting, Station}, _) -> {next_state, add, #station_measurements{station = Station, measurements = []}}.
add(_Event, {adding, Time, Type, Value}, StationMeasurements) ->
  NewMeasurement = #measurement{time = Time, type = Type, value = Value},
  NewStationMeasurements = StationMeasurements#station_measurements{measurements = [NewMeasurement | StationMeasurements#station_measurements.measurements]},
  {next_state, add, NewStationMeasurements};
add(_Event, storing, StationMeasurements) ->
  [pollution_gen_server:add_value(
    StationMeasurements#station_measurements.station,
    Measurement#measurement.time,
    Measurement#measurement.type,
    Measurement#measurement.value) || Measurement <- StationMeasurements#station_measurements.measurements],
  {next_state, set, []}.

%% HANDLERS