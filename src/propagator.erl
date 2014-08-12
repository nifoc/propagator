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
% @doc This is the main module of the Propagator application: It provides the public API.

-module(propagator).

-compile({no_auto_import, [statistics/1]}).

% Types

-type group() :: atom().
-type tag() :: atom().

-export_type([
  group/0,
  tag/0
]).

% API
-export([
  start/0,
  stop/0,
  create/1,
  delete/1,
  publish/3,
  subscribe/1,
  subscribe/2,
  unsubscribe/1,
  unsubscribe/2,
  groups/0,
  is_group/1,
  subscribers/1,
  is_subscriber/2,
  statistics/1
]).

% API

% @doc Starts the Propagator application and all of its dependencies. This is really only meant for usage inside the console.
-spec start() -> ok.
start() ->
  ok = application:start(propagator),
  ok.

% @doc Stops the Propagator application and all of its dependencies. This is really only meant for usage inside the console.
-spec stop() -> ok.
stop() ->
  ok = application:stop(propagator),
  ok.

% @doc Creates a new group that processes can publish and/or subscribe to.
-spec create(group()) -> ok.
create(Group) ->
  case is_group(Group) of
    true -> ok;
    false ->
      ok = pg2:create(Group),
      {ok, _Pid} = supervisor:start_child(propagator_sup, [Group]),
      ok
  end.

% @doc Removes an existing group. If the group does not exist, nothing happens.
-spec delete(group()) -> ok.
delete(Group) ->
  try ets:lookup_element(propagator_groups, Group, 2) of
    Pid ->
      ok = pg2:delete(Group),
      ok = supervisor:terminate_child(propagator_sup, Pid),
      ok
  catch
    error:badarg -> ok
  end.

% @doc Publishes `Data' to `Group' and broadcasts it to all subscribers. All messages have to be tagged, so that subscribers
%      can easily pattern-match on the messages they receive.
-spec publish(group(), tag(), term()) -> ok | {error, term()}.
publish(Group, Tag, Data) ->
  try ets:lookup_element(propagator_groups, Group, 2) of
    Pid ->
      _ = Pid ! {send_message, self(), {Group, Tag, Data}},
      ok
  catch
    error:badarg -> {error, {no_such_group, Group}}
  end.

% @doc Subscribes the current processes (`self()') to a group. Delegates to {@link subscribe/2}.
-spec subscribe(group()) -> ok | {error, term()}.
subscribe(Group) ->
  subscribe(Group, self()).

% @doc Subscribes any `Pid' to a given `Group'.
-spec subscribe(group(), pid()) -> ok | {error, term()}.
subscribe(Group, Pid) ->
  case is_group(Group) of
    true ->
      case is_subscriber(Group, Pid) of
        true -> ok;
        false -> pg2:join(Group, Pid)
      end;
    false -> {error, {no_such_group, Group}}
  end.

% @doc Unsubscribes the current processes (`self()') from a group. Delegates to {@link unsubscribe/2}.
-spec unsubscribe(group()) -> ok | {error, term()}.
unsubscribe(Group) ->
  unsubscribe(Group, self()).

% @doc Unsubscribes any `Pid' from a given `Group'.
-spec unsubscribe(group(), pid()) -> ok | {error, term()}.
unsubscribe(Group, Pid) ->
  pg2:leave(Group, Pid).

% @doc Returns a list of all created groups.
-spec groups() -> [group()].
groups() ->
  ets:foldl(fun({Group, _Pid}, Acc) ->
    [Group | Acc]
  end, [], propagator_groups).

% @doc Checks wether or not a given group exists.
-spec is_group(group()) -> boolean().
is_group(Group) ->
  ets:member(propagator_groups, Group).

% @doc Returns a list of all processes that are subscribed to a `Group'.
-spec subscribers(group()) -> [pid()] | {error, term()}.
subscribers(Group) ->
  pg2:get_local_members(Group).

% @doc Checks wether or not `Pid' is subscribed to `Group'.
-spec is_subscriber(group(), pid()) -> boolean().
is_subscriber(Group, Pid) ->
  case subscribers(Group) of
    {error, _Reason} -> false;
    Members -> lists:member(Pid, Members)
  end.

% @doc Returns a property list with statistics about a given group.<br /><br />
%      Currently the following keys are returned:<br />
%      `group': The name of the group<br />
%      `subscriber_count': Current number of subscribers<br />
%      `message_count': Number if unique messages this process (group) sent
-spec statistics(group()) -> {ok, [{atom(), term()}]} | {error, term()}.
statistics(Group) ->
  try ets:lookup_element(propagator_groups, Group, 2) of
    Pid ->
      Ref = make_ref(),
      _ = Pid ! {statistics, self(), Ref},
      receive
        {Ref, Stats} -> {ok, Stats}
      after
        5000 -> {error, timeout}
      end
  catch
    error:badarg -> {error, {no_such_group, Group}}
  end.
