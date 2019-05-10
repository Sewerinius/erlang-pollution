%%%-------------------------------------------------------------------
%%% @author sewerin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 01:06
%%%-------------------------------------------------------------------
-module(pollutionServer).
-author("sewerin").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDeviation/2]).
-export([start/0, stop/0]).
-export([start_link/0]).

-record(monitor, {nameToCoords, coordsToName, nameToMeasures}).

start() ->
  register(ps, spawn(fun() -> init() end)).

start_link() ->
  spawn_link(fun() -> init() end).

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {addStation, PID, {Name, Coords}} ->
      case pollution:addStation(Monitor, Name, Coords) of
        M2 when is_record(M2, monitor) -> PID ! ok,
          loop(M2);
        Err -> PID ! Err,
          loop(Monitor)
      end;
    {addValue, PID, {Identifier, Date, Type, Value}} ->
      case pollution:addValue(Monitor, Identifier, Date, Type, Value) of
        M2 when is_record(M2, monitor) -> PID ! ok,
          loop(M2);
        Err -> PID ! Err,
          loop(Monitor)
      end;
    {removeValue, PID, {Identifier, Date, Type}} ->
      case pollution:removeValue(Monitor, Identifier, Date, Type) of
        M2 when is_record(M2, monitor) -> PID ! ok,
          loop(M2);
        Err -> PID ! Err,
          loop(Monitor)
      end;
    {getOneValue, PID, {Identifier, Date, Type}} ->
      PID ! pollution:getOneValue(Monitor, Identifier, Date, Type),
      loop(Monitor);
    {getStationMean, PID, {Identifier, Type}} ->
      PID ! pollution:getStationMean(Monitor, Identifier, Type),
      loop(Monitor);
    {getDailyMean, PID, {Date, Type}} ->
      PID ! pollution:getDailyMean(Monitor, Date, Type),
      loop(Monitor);
    {getDeviation, PID, {Date, Type}} ->
      PID ! pollution:getDeviation(Monitor, Date, Type),
      loop(Monitor);
    crash ->
      crash(),
      loop(Monitor);
    stop -> ok;
    Data ->
      io:write("Unknown data"),
      io:write(Data),
      loop(Monitor)
  end.

crash() -> 1 / 0.

stop() ->
  ps ! stop.

call(Option, Data) ->
  ps ! {Option, self(), Data},
  receive
    M -> M
  end.

addStation(Name, Coords) ->
  call(addStation, {Name, Coords}).

addValue(Identifier, Date, Type, Value) ->
  call(addValue, {Identifier, Date, Type, Value}).

removeValue(Identifier, Date, Type) ->
  call(removeValue, {Identifier, Date, Type}).

getOneValue(Identifier, Date, Type) ->
  call(getOneValue, {Identifier, Date, Type}).

getStationMean(Identifier, Type) ->
  call(getStationMean, {Identifier, Type}).

getDailyMean(Date, Type) ->
  call(getDailyMean, {Date, Type}).

getDeviation(Date, Type) ->
  call(getDeviation, {Date, Type}).

