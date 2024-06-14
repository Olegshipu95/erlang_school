%%%-------------------------------------------------------------------
%%% @author olegshipu
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jun 2024 18:08
%%%-------------------------------------------------------------------
-module(threading_ex).
-author("olegshipu").

%% API
-export([parallel/0, long_func/0, entry_point/0, my_custom_map/2, apply_fun/3]).
long_func() -> io:format("Start func~p~n", [self()]),
  timer:sleep(1000 * rand:uniform(10)),
  io:format("End calc~p~n", [self()]).

parallel() -> [spawn(?MODULE, long_func, []) || _ <- lists:seq(1, 5)].





-spec entry_point() -> NewList when NewList :: [A], A :: term().
entry_point() ->
  MyList = [1, 2, 3, 4, 5],
  Fun = fun(X) -> X * X end,
  my_custom_map(Fun, MyList).


-spec my_custom_map(Fun, List) -> NewList when Fun :: fun((A) -> B),
  List :: [A], NewList :: [B], A :: term(), B :: term().
my_custom_map(Fun, List) ->
  ParentPid = self(),
  ChildProcesses = [spawn(?MODULE, apply_fun, [ParentPid, Fun, X]) || X <- List],
  receiver(length(ChildProcesses), []).

%%-spec apply_fun(Parent, Fun, X) ->
apply_fun(Parent, Fun, X) ->
  Parent ! {self(), Fun(X)}.

receiver(0, Results) ->
  Results;

receiver(N, Results) ->
  receive
    {From, Result} ->
      io:format("Take information from ~p value ~p~n", [From, Result]),
      receiver(N - 1, [Result | Results])
  end.
