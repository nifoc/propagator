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

-module(propagator_group).

-record(group_state, {
  group :: propagator:group(),
  msg_count :: non_neg_integer()
}).

-type state() :: #group_state{}.

% API
-export([
  start_link/1
]).

% Loop
-export([
  loop/1
]).

% API

% @doc Starts a group server process.
-spec start_link(propagator:group()) -> {ok, pid()}.
start_link(Group) ->
  State = #group_state{group=Group, msg_count=0},
  Pid = spawn_link(fun() ->
    process_flag(trap_exit, true),
    loop(State)
  end),
  true = ets:insert(propagator_groups, {Group, Pid}),
  {ok, Pid}.

% Loop

% @hidden
-spec loop(state()) -> no_return().
loop(#group_state{group=Group, msg_count=MsgCount}=State) ->
  State2 = receive
    {send_message, _From, {Group, _Tag, _Data}=Msg} ->
      Members = propagator:subscribers(Group),
      ok = lists:foreach(fun(Member) -> Member ! Msg end, Members),
      State#group_state{msg_count=MsgCount+1};
    {statistics, From, Ref} ->
      Stats = [
        {group, Group},
        {subscriber_count, length(propagator:subscribers(Group))},
        {message_count, MsgCount}
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

-spec terminate(atom(), state()) -> no_return().
terminate(Reason, #group_state{group=Group}) ->
  true = ets:delete(propagator_groups, Group),
  exit(Reason).
