%%%-------------------------------------------------------------------
%%% @author sewerin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Apr 2019 18:08
%%%-------------------------------------------------------------------
-module(pollution).
-author("sewerin").

%% API

-export([createMonitor/0, test/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDeviation/3]).

-record(monitor, {nameToCoords, coordsToName, nameToMeasures}).

createMonitor() -> #monitor{nameToCoords = #{}, coordsToName = #{}, nameToMeasures = #{}}.

identify(Monitor, Identifier) when is_tuple(Identifier) ->
  case maps:find(Identifier, Monitor#monitor.coordsToName) of
    {ok, Val} -> Val;
    error -> noStation
  end;
identify(Monitor, Identifier) when is_list(Identifier) ->
  case maps:find(Identifier, Monitor#monitor.nameToCoords) of
    {ok, _} -> Identifier;
    error -> noStation
  end.

addStation(Monitor, Name, Coords) when is_record(Monitor, monitor) andalso is_list(Name) andalso is_tuple(Coords) ->
  #monitor{nameToCoords = NameToCoords, coordsToName = CoordsToName, nameToMeasures = NameToMeasures} = Monitor,
  case maps:find(Name, NameToCoords) of
    error ->
      case maps:find(Coords, CoordsToName) of
        error ->
          Monitor#monitor{nameToCoords = NameToCoords#{Name => Coords}, coordsToName = CoordsToName#{Coords => Name}, nameToMeasures = NameToMeasures#{Name => #{}}};
        _ -> coordUsedWithDiffrentName
      end;
    {ok, Val} ->
      if
        Val == Coords -> Monitor;
        true -> nameUsedWithDiffrentCoord
      end
  end.

addValue(Monitor, Identifier, Date, Type, Value) when is_record(Monitor, monitor) andalso is_list(Type) andalso is_tuple(Date) ->
  case identify(Monitor, Identifier) of
    noStation -> noStation;
    Name ->
      NameToMeasures = Monitor#monitor.nameToMeasures,
      Measures = maps:get(Name, NameToMeasures),
      case maps:find({Date, Type}, Measures) of
        {ok, _} -> used;
        error -> Monitor#monitor{nameToMeasures = NameToMeasures#{Name => Measures#{{Date, Type} => Value}}}
      end
  end.

removeValue(Monitor, Identifier, Date, Type) ->
  case identify(Monitor, Identifier) of
    noStation -> noStation;
    Name ->
      NameToMeasures = Monitor#monitor.nameToMeasures,
      Measures = maps:get(Name, NameToMeasures),
      Monitor#monitor{nameToMeasures = NameToMeasures#{Name => maps:remove({Date, Type}, Measures)}}
  end.

getOneValue(Monitor, Identifier, Date, Type) ->
  case identify(Monitor, Identifier) of
    noStation -> noStation;
    Name ->
      Measures = maps:get(Name, Monitor#monitor.nameToMeasures),
      case maps:find({Date, Type}, Measures) of
        {ok, Val} -> Val;
        error -> noValue
      end
  end.

getStationMean(Monitor, Identifier, Type) ->
  case identify(Monitor, Identifier) of
    noStation -> noStation;
    Name ->
      NameToMeasures = Monitor#monitor.nameToMeasures,
      Measures = maps:get(Name, NameToMeasures),
      M1 = maps:values(maps:filter(fun({_, T}, _) -> T == Type end, Measures)),
      lists:sum(M1) / length(M1)
  end.

getAllMeasures(Monitor) ->
  NameToMeasures = Monitor#monitor.nameToMeasures,
  Measures = maps:values(NameToMeasures),
  L1 = lists:map(fun maps:to_list/1, Measures),
  lists:append(L1).

getDailyMean(Monitor, {Date, _}, Type) ->
  getDailyMean(Monitor, Date, Type);
getDailyMean(Monitor, Date, Type) ->
  L2 = getAllMeasures(Monitor),
  L3 = lists:filter(fun({{{D, _}, T}, _}) -> (D == Date) and (T == Type) end, L2),
  L4 = lists:map(fun({_, X}) -> X end, L3),
  case length(L4) of
    0 -> noMeasures;
    Len -> lists:sum(L4) / Len
  end.

getDeviation(Monitor, {Date, {Hour, _, _}}, Type) ->
  L2 = getAllMeasures(Monitor),
  L3 = lists:filter(fun({{{D, {H, _, _}}, T}, _}) -> (D == Date) and (H == Hour) and (T == Type) end, L2),
  L4 = lists:map(fun({_, X}) -> X end, L3),
  case length(L4) of
    0 -> noMeasures;
    Len ->
      Avg = lists:sum(L4)/Len,
      L5 = lists:map(fun(X) -> math:pow(Avg - X, 2) end, L4),
      math:sqrt(lists:sum(L5) / Len)
  end.

test() ->
  M1 = createMonitor(),
  M2 = addStation(M1, "asd", {1, 2}),
  M2_5 = addStation(M2, "as", {2, 3}),
  M3 = addValue(M2_5, "asd", calendar:gregorian_seconds_to_datetime(1), "T", 2),
  M3_5 = addValue(M3, "asd", calendar:gregorian_seconds_to_datetime(1), "PM10", 2),
  M4 = addValue(M3_5, {1, 2}, calendar:gregorian_seconds_to_datetime(2), "T", 4),
  M5 = addValue(M4, "as", calendar:gregorian_seconds_to_datetime(1), "T", 4),
  V1 = getDeviation(M5, {{0, 1, 1}, {0, 0, 1}}, "T"),
  io:format("~w~n", [V1]).
%%  V2 = getDailyMean(M5, calendar:gregorian_days_to_date(1), "T"),
%%  io:format("~w~n", [V2]).

