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

-include_lib("eunit/include/eunit.hrl").

%% API
-export([connect/0, handle_connection/0, get_all_pids/0, test_server/0]).

init_ets() ->
  ets:new(connection_pids, [set, named_table, public]).

connect() ->
  case ets:info(connection_pids) of
    undefined -> init_ets();
    _ -> io:format("Exit proc ~p", [self()]),
      ok
  end,
  Pid = spawn(?MODULE, handle_connection, []),
  ets:insert(connection_pids, {Pid, Pid}),
  Pid.




handle_connection() ->
  receive
    {calculate, From, Data} when is_pid(From) andalso is_tuple(Data) ->
      io:format("Take information from ~p value ~p~n", [From, Data]),
      Result = operation_validation(Data),
      From ! {self(), Result},
      handle_connection();
    stop ->
      ok;
    pids ->
      get_all_pids(),
      handle_connection();
    _ ->
      handle_connection()
  end.


operation_validation({RequestOperation, Args})  when is_atom(RequestOperation) andalso is_list(Args)->
  {Code, MyOperation} = operation(RequestOperation),
  case Code of
    ok ->
      io:format("My operation is - ~p, my args - ~p~n", [MyOperation, Args]),
      {ok, divider_args(MyOperation, Args, 0)};
    _  -> {Code, MyOperation}
  end;
operation_validation(Data) -> {error, io_lib:format("unknown data ~p~n", [Data])}.

divider_args(_, [], Acc) -> Acc;
divider_args(Operation, [A|Tail], Acc) ->
  io:format("Step_divider: Acc is ~p, A is ~p, Tail is ~p~n", [Acc, A, Tail]),
  divider_args(Operation, Tail, erlang:apply(erlang, Operation, [A,Acc])).


operation(add) -> {ok, '+'};
operation(sub) -> {ok, '-'};
operation(mul) -> {ok, '*'};
operation(division) -> {ok, '/'};
operation(remainder) -> {ok, 'rem'};
operation(AnotherAtom) ->
  {error, io_lib:format("Invalid operation: ~p~n", [AnotherAtom])}.


get_all_pids() ->
  case ets:info(connection_pids) of
    undefined -> init_ets();
    _ -> ok
  end,
  Pids = ets:tab2list(connection_pids),
  [Pid || {Pid, _} <- Pids].





%% TEST_PART

calculate(Pid, Request) ->
  Pid ! {calculate, erlang:self(), Request},
  receive
    {Pid, Result} ->
      io:format("Answer from Pid: ~p is - ~p~n", [Pid, Result]),
      Result
  after 5000 ->
    {error, timeout}
  end.

test_server() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  calculate(Pid, {add, [1, 2, 3.06, -1]}),
  Pid ! exit.