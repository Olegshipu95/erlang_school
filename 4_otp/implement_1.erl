-module(implement_1).
-behaviour(sample_behaviour).

-export([init/1,
  action/1
]).

-record(state,{list :: [integer()]}).

init(Args) ->
  {ok, #state{list = Args}}.

action(State = #state{list = []})->
  {ok, empty, State};
action(State = #state{list = [Head|Rest]})->
  {ok, Head, State#state{list = Rest}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test()->
  {ok, Opaq1} = implement_1:init([1,2,3]),
  {ok, 1, Opaq2} = implement_1:action(Opaq1),
  {ok, 2, Opaq3} = implement_1:action(Opaq2),
  {ok, 3, Opaq4} = implement_1:action(Opaq3),
  {ok, empty, Opaq5} = implement_1:action(Opaq4),

  {ok, default} = sample_behavoiur:default(implement_1, Opaq4).
-endif.