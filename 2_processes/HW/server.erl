%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2024 20:12
%%%-------------------------------------------------------------------
-module(server).
-author("oleg").

%% API
-export([connect/0, handle_connection/0, print_pids/0]).


connect() ->
  Pid = spawn(?MODULE, handle_connection, []),
  register(Pid, ?MODULE),
  add_pid_to_list(Pid).

handle_connection() ->
  receive
    {From, Data} when is_pid(From) andalso is_tuple(Data) ->
      io:format("Take information from ~p value ~p~n", [From, Data]),
      handle_connection()
    after 1000 -> handle_connection()
  end.

add_pid_to_list(Pid) ->
  PidList = case whereis(?MODULE) of
              undefined -> [];
              Pid -> ets:lookup(?MODULE, pid_list)
            end,
  ets:insert(?MODULE, {pid_list, [Pid | PidList]}).

print_pids() ->
  PidList = ets:lookup(?MODULE, pid_list),
  io:format("Current Pids: ~p~n", [PidList]).
