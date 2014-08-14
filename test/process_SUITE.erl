% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(process_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% Tests
-export([
  subscribed/1,
  publish/1,
  stats/1,
  kill_subscriber/1
]).

% Common Test

all() ->
  [
    subscribed,
    publish,
    stats,
    kill_subscriber
  ].

init_per_suite(Config) ->
  ok = propagator:start(),
  ok = propagator:create(test),
  Pid = spawn(process_sub, init, [test]),
  [{proc, Pid} | Config].

end_per_suite(_Config) ->
  ok = propagator:stop(),
  ok.

% Tests

subscribed(Config) ->
  Pid = ?config(proc, Config),
  [Pid] = propagator:subscribers(test),
  true = propagator:is_subscriber(test, Pid).

publish(_Config) ->
  ok = propagator:publish(test, test, "test").

stats(_Config) ->
  {ok, Stats} = propagator:statistics(test),
  {group, test} = lists:keyfind(group, 1, Stats),
  {subscriber_count, 1} = lists:keyfind(subscriber_count, 1, Stats),
  {message_count, 1} = lists:keyfind(message_count, 1, Stats).

kill_subscriber(Config) ->
  Pid = ?config(proc, Config),
  1 = length(propagator:subscribers(test)),
  _ = Pid ! terminate,
  ok = timer:sleep(500), % Takes a bit for the process to terminate and pg2 to notice
  [] = propagator:subscribers(test),
  false = propagator:is_subscriber(test, Pid).
