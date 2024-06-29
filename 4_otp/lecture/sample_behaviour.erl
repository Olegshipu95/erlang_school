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

-callback init(Args :: list())->
  {ok, Pid :: pid()}.

-callback action(Pid :: pid(), Opcode :: atom(), Args :: tuple())->
  {ok, ActionResult :: term()} | {error, ErrorInfo :: term()}.