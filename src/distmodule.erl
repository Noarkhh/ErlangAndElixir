%%%-------------------------------------------------------------------
%%% @author noarkhh
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Mar 2023 15:51
%%%-------------------------------------------------------------------
-module(distmodule).
-author("noarkhh").

%% API
-export([generate_lockers/1, generate_people/1, find_min_distance/2, find_min_distance_concurrent/2, find_closest_to_locker/3]).

dist({X1, Y1}, {X2, Y2}) ->
  DX = abs(X1 - X2),
  DY = abs(Y1 - Y2),
  math:sqrt(DX * DX + DY * DY).

generate_lockers(LockersNum) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, LockersNum)].

generate_people(PeopleNum) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, PeopleNum)].

find_closest_to_locker(LockerLocation, PeopleLocations, ParentPid) ->
  ParentPid ! lists:min([{dist(P, LockerLocation), {P, LockerLocation}} || P <- PeopleLocations]).

find_min_distance_concurrent(PeopleLocations, LockerLocations) ->
  [spawn(?MODULE, fun find_closest_to_locker/3, [L, PeopleLocations, self()]) || L <- LockerLocations],
  lists:min([receive M -> M end || _ <- LockerLocations]).

find_min_distance(PeopleLocations, LockerLocations) ->
  lists:min([{dist(P, L), {P, L}} || P <- PeopleLocations, L <- LockerLocations]).


