-module(bf_tsdb_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    {setup, local,
     fun start_process/0,
     fun stop_process/1,
     fun run/1}.

start_process() ->
    application:set_env(bf_tsdb,  tsdb_root, "."),
    application:set_env(bf_tsdb,  log4erl_config, "../etc/log4erl.conf"),
    bf_tsdb_app:start().
 
stop_process(_P) ->
    application:unset_env(bf_tsdb, tsdb_root),
    bf_tsdb_app:stop(),
    ok.

run(_P) ->
    [
     ?_assertMatch(ok, bf_tsdb:open("test_db"))
     %% ?_assertMatch("../scripts", ec_util:scripts_dir()),
     %% ?_assertMatch("../config", ec_util:config_dir()),
    ].


