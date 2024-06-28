%%%-------------------------------------------------------------------
%%% @author oleg
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2024 20:12
%%%-------------------------------------------------------------------
-module(bd).
-author("oleg").

%% API
-export([new/0, destroy/1, save/2, delete/2, read/2, match/2]).

new() -> spawn(fun() -> bd_init() end).

-spec bd_init() -> ok.
bd_init() -> bd_worker(#{}).

-spec bd_worker(map()) -> ok.
bd_worker(Storage) when is_map(Storage) ->
  {UpdateStorage, ID, RespCode, RespBody} =
    receive

      {save, From, {Key, Value} = Pair} when is_pid(From) ->
        NewStorage = maps:put(Key, Value, Storage),
        {NewStorage, From, ok, Pair};

      {read, From, Key} when is_pid(From) ->
        FoundElement = maps:find(Key, Storage),
        {Storage, From, ok, FoundElement};

      {delete, From, Key} when is_pid(From) ->
        NewStorage = maps:remove(Key, Storage),
        {NewStorage, From, ok, Key};

      {match, From, Value} when is_pid(From) ->
        MatchKeys = maps:filter(fun(_K, V) -> Value == V end, Storage),
        {Storage, From, ok, MatchKeys};

      stop -> ok

    end,
  ID ! {self(), RespCode, RespBody},
  bd_worker(UpdateStorage).

-spec destroy(pid()) -> ok.
destroy(Pid) when is_pid(Pid) -> exit(Pid, "got exit signal").

-spec save({Key :: term(), Value :: term()}, pid()) -> term().
save({_Key, _Value} = Pair, Pid) when is_pid(Pid) -> bd_client(Pid,{save, Pair}).

-spec delete(term(), pid()) -> term().
delete(Key, Pid) when is_pid(Pid) -> bd_client(Pid, {delete, Key}).

-spec read(term(), pid()) -> term().
read(Key, Pid) when is_pid(Pid) -> bd_client(Pid, {read, Key}).

-spec match(term(), pid()) -> term().
match(Key, Pid) when is_pid(Pid) -> bd_client(Pid, {match, Key}).

bd_client(Pid, {Operation, Args}) when is_atom(Operation) andalso is_pid(Pid) ->
  BdRequest = {Operation, self(), Args},
  Pid ! BdRequest,

  receive
    {BdPid, Atom, _} = Resp when is_pid(BdPid) andalso is_atom(Atom) -> Resp
    after 5000 -> error
  end.