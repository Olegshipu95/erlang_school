%%%-------------------------------------------------------------------
%%% @author olegshipu
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2024 17:49
%%%-------------------------------------------------------------------
-module(sample_behaviour).
-author("olegshipu").
-export([default/2]).

-callback init(Args :: list())->
  {ok, Pid :: pid()}.

-callback action(Pid :: pid(), Opcode :: atom(), Args :: tuple())->
  {ok, ActionResult :: term()} | {error, ErrorInfo :: term()}.

-spec default(Mod :: atom(), State :: term())-> {ok, DefaultResult :: term() }.
default(Mod, State)->
  case erlang:function_exported(Mod, default, 1) of
    true ->
      Mod:default(State);
    false ->
      {ok, default}
  end.