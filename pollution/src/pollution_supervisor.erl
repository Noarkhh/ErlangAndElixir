%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2023 19:51
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-author("noarkhh").
-behavior(supervisor).

%% API
-export([init/1, start_link/1]).

start_link(Mode) when (Mode == test) or (Mode == normal) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Mode).
%%  unlink(whereis(?MODULE)).

init(Mode) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => 3,
    period => 5
  },
  ChildSpecList = [
    #{
      id => pollution_gen_server,
      start => {pollution_gen_server, start_link, [Mode]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [pollution_gen_server]
    },
    #{
      id => pollution_value_collector_gen_statem,
      start => {pollution_value_collector_gen_statem, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [pollution_value_collector_gen_statem]
    }
  ],
  {ok, {SupFlags, ChildSpecList}}.