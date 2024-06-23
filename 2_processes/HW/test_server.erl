%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2024 20:12
%%%-------------------------------------------------------------------
-module(test_server).
-author("oleg").

%% API
-export([test_server/0]).
-import(server, [connect/0, print_pids/0]).

test_server() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  Pid ! {self(), {1, 2, 3}}.
