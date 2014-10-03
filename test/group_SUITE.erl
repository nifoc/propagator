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
  subscriber_actions/1,
  messages/1,
  no_messages/1,
  stats/1,
  callbacks/1,
  ignore_unknown_messages/1
]).

% Common Test

all() ->
  [
    groups_actions,
    subscriber_actions,
    messages,
    no_messages,
    stats,
    callbacks,
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

subscriber_actions(_Config) ->
  Pid = self(),
  [] = propagator:subscribers(test),
  {error, {no_such_group, temp}} = propagator:subscribers(temp),
  ok = propagator:subscribe(test),
  {error, {no_such_group, temp}} = propagator:subscribe(temp),
  [Pid] = propagator:subscribers(test),
  ok = propagator:subscribe(test, Pid),
  [Pid] = propagator:subscribers(test),
  true = propagator:is_subscriber(test, Pid),
  false = propagator:is_subscriber(temp, Pid),
  {error, {no_such_group, temp}} = propagator:unsubscribe(temp),
  ok = propagator:unsubscribe(test),
  [] = propagator:subscribers(test),
  ok = propagator:unsubscribe(test, Pid),
  [] = propagator:subscribers(test),
  false = propagator:is_subscriber(test, Pid).

messages(_Config) ->
  ok = propagator:subscribe(test),
  Ref = make_ref(),
  {error, {no_such_group, temp}} = propagator:publish(temp, test, Ref),
  ok = propagator:publish(test, test, Ref),
  ok = receive
    {test, test, Ref} -> ok;
    _ -> error
  after
    2000 -> error
  end.

no_messages(_Config) ->
  ok = propagator:subscribe(test),
  Ref = make_ref(),
  ok = propagator:publish(test, test, Ref),
  ok = receive
    {test, test, Ref} -> ok;
    _ -> error
  after
    2000 -> error
  end,
  ok = propagator:unsubscribe(test),
  ok = propagator:publish(test, test, Ref),
  ok = receive
    {test, test, Ref} -> error;
    _ -> error
  after
    2000 -> ok
  end.

stats(_Config) ->
  {error, {no_such_group, temp}} = propagator:statistics(temp),
  {ok, Stats} = propagator:statistics(test),
  {group, test} = lists:keyfind(group, 1, Stats),
  true = proplists:get_value(message_count, Stats) > 0.

callbacks(_Config) ->
  ok = propagator:create(test_cb1),
  ok = propagator:publish(test_cb1, test, test),
  ok = application:set_env(propagator, callback, []),
  ok = propagator:create(test_cb2),
  ok = propagator:publish(test_cb2, test, test).

ignore_unknown_messages(_Config) ->
  Pid = ets:lookup_element(propagator_groups, test, 2),
  Pid ! foo.
