-module(implement_1).
-behaviour(sample_behaviour).

-export([init/1,
  action/3
]).

-record(state, {list :: [integer()]}).

init(Args) ->
  {ok, #state{list = Args}}.

action(Pid, read, _) ->
  Pid ! read,
  receive
    {ok, _ActionResult} = Result -> Result;
    {error, _ErrorInfo} = Error -> Error
  end.

server_init(Args) when is_list(Args) -> server_handler(Args).
server_handler(Storage = [Head | Tail]) ->
  NewStorage = receive
                 {read, From} -> From ! Head,
                   Tail
               end,
  server_handler(NewStorage).


  -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, Opaq1} = implement_1:init([1, 2, 3]),
  {ok, 1, Opaq2} = implement_1:action(Opaq1),
  {ok, 2, Opaq3} = implement_1:action(Opaq2),
  {ok, 3, Opaq4} = implement_1:action(Opaq3),
  {ok, empty, Opaq5} = implement_1:action(Opaq4),
  -endif.