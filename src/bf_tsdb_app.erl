-module(bf_tsdb_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-define(APPS, [log4erl, bf_tsdb]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% to start manually from console with start.sh
start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    Config = bf_tsdb_util:log4erl_config(),
    log4erl:conf(Config),
    log4erl:info("starting bf_tsdb"),
    bf_tsdb_sup:start_link().

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping bf_tsdb"),
    [application:stop(A) || A <- lists:reverse(?APPS)].
