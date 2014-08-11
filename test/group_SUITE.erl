% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(group_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% Tests
-export([
  groups_actions/1,
  member_actions/1,
  messages/1,
  stats/1,
  ignore_unknown_messages/1
]).

% Common Test

all() ->
  [
    groups_actions,
    member_actions,
    messages,
    stats,
    ignore_unknown_messages
  ].

init_per_suite(Config) ->
  ok = propagator:start(),
  ok = propagator:create(test),
  Config.

end_per_suite(_Config) ->
  ok = propagator:stop(),
  ok.

% Tests

groups_actions(_Config) ->
  [test] = propagator:groups(),
  ok = propagator:create(test),
  [test] = propagator:groups(),
  ok = propagator:create(temp),
  [test, temp] = propagator:groups(),
  ok = propagator:delete(temp),
  ok = propagator:delete(temp),
  [test] = propagator:groups().

member_actions(_Config) ->
  Pid = self(),
  [] = propagator:members(test),
  {error, {no_such_group, temp}} = propagator:members(temp),
  ok = propagator:subscribe(test),
  {error, {no_such_group, temp}} = propagator:subscribe(temp),
  [Pid] = propagator:members(test),
  ok = propagator:subscribe(test, Pid),
  [Pid] = propagator:members(test),
  true = propagator:is_member(test, Pid),
  false = propagator:is_member(temp, Pid),
  {error, {no_such_group, temp}} = propagator:unsubscribe(temp),
  ok = propagator:unsubscribe(test),
  [] = propagator:members(test),
  ok = propagator:unsubscribe(test, Pid),
  [] = propagator:members(test),
  false = propagator:is_member(test, Pid).

messages(_Config) ->
  ok = propagator:subscribe(test),
  Ref = make_ref(),
  {error, {no_such_group, temp}} = propagator:publish(temp, test, Ref),
  ok = propagator:publish(test, test, Ref),
  ok = receive
    {test, test, Ref} -> ok;
    _ -> error
  after
    5000 -> error
  end.

stats(_Config) ->
  {error, {no_such_group, temp}} = propagator:statistics(temp),
  {ok, Stats} = propagator:statistics(test),
  {group, test} = lists:keyfind(group, 1, Stats),
  true = proplists:get_value(message_count, Stats) > 0.

ignore_unknown_messages(_Config) ->
  Pid = ets:lookup_element(propagator_groups, test, 2),
  Pid ! foo.
