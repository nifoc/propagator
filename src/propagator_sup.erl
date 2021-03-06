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
% @hidden
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc Supervisor of the Propagator application.

-module(propagator_sup).
-behaviour(supervisor).

% API
-export([start_link/0]).

% supervisor
-export([init/1]).

% API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% supervisor

init([]) ->
  propagator_groups = ets:new(propagator_groups, [set, public, named_table, {read_concurrency, true}]),
  Spec = [{propagator_group,
    {propagator_group, start_link, []},
    permanent,
    5000,
    worker,
    [propagator_group]
  }],
  {ok, {{simple_one_for_one, 5, 10}, Spec}}.
