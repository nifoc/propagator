% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc A `propagator_group' process is responsible for sending messages to all members of a group.
%      Sending is done using a separate process so that the publisher is never blocked.

-module(propagator_group).

-record(group_state, {
  group :: propagator:group(),
  msg_count :: non_neg_integer(),
  callback :: callback()
}).

-type state() :: #group_state{}.

-type callback() :: {module(), atom()} | undefined.

% API
-export([
  start_link/1
]).

% Loop
-export([
  init/1,
  loop/1
]).

% API

% @doc Starts a group server process.
-spec start_link(propagator:group()) -> {ok, pid()}.
start_link(Group) ->
  Pid = spawn_link(?MODULE, init, [Group]),
  true = ets:insert(propagator_groups, {Group, Pid}),
  {ok, Pid}.

% Loop

% @hidden
-spec init(propagator:group()) -> no_return().
init(Group) ->
  _ = process_flag(trap_exit, true),
  Callback = application:get_env(propagator, callback),
  State = #group_state{group=Group, msg_count=0, callback=Callback},
  loop(State).

% @hidden
-spec loop(state()) -> no_return().
loop(#group_state{group=Group, msg_count=MsgCount, callback=Callback}=State) ->
  State2 = receive
    {send_message, From, {Group, _Tag, _Data}=Msg} ->
      Members = propagator:subscribers(Group),
      ok = lists:foreach(fun(Member) -> Member ! Msg end, Members),
      ok = maybe_invoke_callback(Callback, From, Msg),
      State#group_state{msg_count=MsgCount+1};
    {statistics, From, Ref} ->
      {message_queue_len, MsgQueue} = process_info(self(), message_queue_len),
      Stats = [
        {group, Group},
        {subscriber_count, length(propagator:subscribers(Group))},
        {message_count, MsgCount},
        {message_queue, MsgQueue}
      ],
      _ = From ! {Ref, Stats},
      State;
    {terminate, _From} -> terminate(normal, State);
    {'EXIT', _From, Reason} -> terminate(Reason, State);
    Msg ->
      ok = error_logger:error_msg("Propagator group received unexpected message: ~p~n", [Msg]),
      State
  end,
  ?MODULE:loop(State2).

% Private

-spec maybe_invoke_callback(callback(), pid(), {propagator:group(), propagator:tag(), term()}) -> ok.
maybe_invoke_callback(undefined, _From, _Msg) ->
  ok;
maybe_invoke_callback({Mod, Fun}, From, Msg) ->
  _ = apply(Mod, Fun, [From, Msg]),
  ok.

-spec terminate(atom(), state()) -> no_return().
terminate(Reason, #group_state{group=Group}) ->
  true = ets:delete(propagator_groups, Group),
  exit(Reason).
