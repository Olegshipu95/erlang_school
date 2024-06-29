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
-export([connect/0, handle_connection/0, get_all_pids/0]).

-spec init_ets() -> atom().
init_ets() ->
  ets:new(connection_pids, [set, named_table, public]).

-spec connect() -> pid().
connect() ->
  case ets:info(connection_pids) of
    undefined -> init_ets();
    _ -> io:format("Exit proc ~p", [self()]),
      ok
  end,
  Pid = spawn(?MODULE, handle_connection, []),
  ets:insert(connection_pids, {Pid, Pid}),
  Pid.

-spec handle_connection() -> ok.
handle_connection() ->
  receive
    {calculate, From, Data} when is_pid(From) andalso is_tuple(Data) ->
      io:format("Take information from ~p value ~p~n", [From, Data]),
      Result = success_operation(Data),
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

-spec success_operation({atom(), list()}) -> {ok, any()} | {error, any()}.
success_operation(Data = {RequestOperation, Args}) when is_atom(RequestOperation) andalso is_list(Args) ->
  ReturnResult = try operation_validation(Data) of
                   ResultOfOperation -> {ok, ResultOfOperation}
                 catch
                   Throw -> {error, Throw}
                 end,
  ReturnResult;
success_operation(Data) -> {error, io_lib:format("unknown data ~p~n", [Data])}.

-spec operation_validation({atom(), list()}) -> any();
    (integer()) -> integer();
    (any()) -> no_return().
operation_validation({RequestOperation, Args}) when is_atom(RequestOperation) andalso is_list(Args) ->
  {Code, Fun} = operation(RequestOperation),
  Result = case Code of
             ok ->
               io:format("My operation is - ~p, my args - ~p~n", [Fun, Args]),
               Fun(Args);
             Error -> io:format("Error - ~p~n", [Error]),
               throw(incorrect_operation)
           end,
  Result;
operation_validation(X) when is_number(X) -> X;
operation_validation(Error) -> io:format("Error in operation validation - ~p~n", [Error]),
  throw(incorrect_data).


-spec operation(atom()) -> {ok, fun((list()) -> any())} | {error, list()}.
operation(add) -> {ok, fun(Args) -> lists:foldl(fun(Arg, Acc) -> operation_validation(Arg) + Acc end, 0, Args) end};
operation(sub) ->
  {ok, fun(Args) -> lists:foldl(fun(Arg, Acc) -> Acc - operation_validation(Arg) end, hd(Args), tl(Args)) end};
operation(mul) -> {ok, fun(Args) -> lists:foldl(fun(Arg, Acc) -> operation_validation(Arg) * Acc end, 1, Args) end};
operation(division) ->
  {ok, fun(Args) -> lists:foldl(fun(Arg, Acc) -> Acc / operation_validation(Arg) end, hd(Args), tl(Args)) end};
operation(Data) ->
  {error, io_lib:format("Invalid operation: ~p~n", [Data])}.

-spec get_all_pids() -> [pid()].
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

add_test() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  ?_assertEqual(5.06, calculate(Pid, {add, [1, 2, 3.06, -1]})),
  Pid ! exit.

sub_test() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  ?_assertEqual(4, calculate(Pid, {sub, [10, 1, 2, 3]})),
  Pid ! exit.

mul_test() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  ?_assertEqual(120, calculate(Pid, {mul, [1, 2, 3, 4, 5]})),
  Pid ! exit.

polish_notation_test() ->
  Pid = server:connect(),
  io:format("~p: send to ~p~n", [self(), Pid]),
  ?_assertEqual(5, calculate(Pid, {add, [1, 2, 3, {sub, [4, 5]}]})),
  Pid ! exit.