# Propagator

Pub/Sub for Erlang processes.

[![Build Status](https://travis-ci.org/nifoc/propagator.png)](https://travis-ci.org/nifoc/propagator) [![Coverage Status](https://coveralls.io/repos/nifoc/propagator/badge.png?branch=master)](https://coveralls.io/r/nifoc/propagator?branch=master)

## Status

This is alpha software. Things might still change in ways that break everything.

## Usage

**While `pg2` is used internally, having groups span multiple nodes is not supported.**

### Working with groups

```erlang
ok = propagator:create(demo),
AllGroups = propagator:groups(), % => [demo]
ok = propagator:delete(demo),
false = propagator:is_group(demo).
```

In order to subscribe to a group, it has to be created first. This can be done using the `propagator:create/1` method. If at any point you need to remove a group again, you do that using `propagator:delete/1`.

You can check if a process is subscribed to a group using `propagator:is_subscriber/2` and get a list of all subscribers by calling `propagator:subscribers/1`.

```erlang
IsSubscriber = propagator:is_subscriber(demo, self()), % => true | false
Subscribers = propagator:subscribers(demo). % => […]
```

You can get some basic statistics about a group by calling `propagator:statistics/1`.

### Publish/Subscribe

Assuming you have created a group called `demo`, you can subscribe any process to it using `propagator:subscribe/2`. The method `propagator:subscribe/1` also exists to subscribe `self()` to a group.

```erlang
ok = propagator:subscribe(demo).
```

Publishing a message to a group is done by calling `propagator:publish/3`. The publishing process does not need to be subscribed to the group.

```erlang
ok = propagator:publish(demo, some_tag, [{any, data}]).
```

All messages are tagged, so that subscribing processes can easily pattern-match of incoming messages. The third argument can be anything you want.

You can unsubscribe any process using the `propagator:unsubscribe/2` method. `propagator:unsubscribe/1` also exists to remove the subscription of `self()`.

```erlang
ok = propagator:unsubscribe(demo).
```

## License

[ISC](https://en.wikipedia.org/wiki/ISC_license).

```
Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
```
