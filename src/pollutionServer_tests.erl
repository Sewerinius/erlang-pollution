%%%-------------------------------------------------------------------
%%% @author sewerin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 10:15
%%%-------------------------------------------------------------------
-module(pollutionServer_tests).
-author("sewerin").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

addValue_test() ->
  D = calendar:local_time(),
  pollutionServer:start(),
  noStation = pollutionServer:addValue({4, 2}, D, "T", 12),
  ok = pollutionServer:addStation("Station1", {4, 2}),
  ok = pollutionServer:addValue({4, 2}, D, "T", 12),
  used = pollutionServer:addValue("Station1", D, "T", 12),
  pollutionServer:stop().

addStation_test() ->
  pollutionServer:start(),
  ok = pollutionServer:addStation("Station1", {4, 2}),
  coordUsedWithDiffrentName = pollutionServer:addStation("Station2", {4, 2}),
  nameUsedWithDiffrentCoord = pollutionServer:addStation("Station1", {7, 6}),
  pollutionServer:stop().

removeValue_test() ->
  D = calendar:local_time(),
  pollutionServer:start(),
  ok = pollutionServer:addStation("Station1", {4, 2}),
  ok = pollutionServer:addValue({4, 2}, D, "T", 12),
  ok = pollutionServer:removeValue({4, 2}, D, "T"),
  pollutionServer:stop().

getOneValue_test() ->
  D = calendar:local_time(),
  pollutionServer:start(),
  noStation = pollutionServer:addValue({4, 2}, D, "T", 12),
  ok = pollutionServer:addStation("Station1", {4, 2}),
  pollutionServer:addValue({4, 2}, D, "T", 12),
  C = pollutionServer:getOneValue({4, 2}, D, "T"),
  pollutionServer:stop(),
  ?assert(C == 12).

getStationMean_test() ->
  D = calendar:local_time(),
  pollutionServer:start(),
  pollutionServer:addStation("Station1", {4, 2}),
  pollutionServer:addValue({4, 2}, D, "T", 12),
  pollutionServer:addValue({4, 2}, calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}), "T", 10),
  C = pollutionServer:getStationMean("Station1", "T"),
  pollutionServer:stop(),
  ?assert(C == 11).


getDailyMean_test() ->
  D = calendar:local_time(),
  pollutionServer:start(),
  pollutionServer:addStation("Station1", {4, 2}),
  pollutionServer:addStation("Station2", {7, 6}),
  pollutionServer:addValue({4, 2}, D, "T", 12),
  pollutionServer:addValue({7, 6}, D, "T", 10),
  C = pollutionServer:getDailyMean(D, "T"),
  pollutionServer:stop(),
  ?assertEqual(C, 11).