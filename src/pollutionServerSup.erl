%%%-------------------------------------------------------------------
%%% @author sewerin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 12:14
%%%-------------------------------------------------------------------
-module(pollutionServerSup).
-author("sewerin").
%%-behaviour(supervisor).

%% API
%%-export([start_link/1, init/1, stop/1]).
-export([start/1]).

start(Alias) ->
  register(Alias, pollutionServer:start_link()),
  process_flag(trap_exit, true),
  receive
    {'EXIT', Pid, normal} -> ok;
    {'EXIT', Pid, Reason} -> io:write('respowning client~n') ,start(Alias);
    _ -> err
  end.

crash(Alias) -> Alias ! {crash}.

%%start_link(Args) ->
%%  supervisor:start_link({local, psSup}, ?MODULE, Args)
%%
%%init(Args) ->
%%  {ok, {
%%    {one_for_one, 2, 3},
%%    [{ps, {pollutionServer, start, []}, transient, infinity, worker}]
%%  }}.