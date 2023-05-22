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
-export([init/1, callback_mode/0, start_link/0, stop/0]).

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_statem:stop(?MODULE).

set_station(Station) -> gen_statem:cast(?MODULE, {setting, Station}).
add_value(Time, Type, Value) -> gen_statem:cast(?MODULE, {adding, Time, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, storing).

init([]) ->
  {ok, set, []}.
callback_mode() -> state_functions.

set(_Event, {setting, Station}) -> {next_state, add, }

%% HANDLERS