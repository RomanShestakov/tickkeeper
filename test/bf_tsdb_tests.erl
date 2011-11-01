-module(bf_tsdb_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA, [{timestamp, integer}, {bid, float}, {ask, float}]).


process_test_() ->
    {setup, local,
     fun start_process/0,
     fun stop_process/1,
     fun run/1}.

start_process() ->
    bf_tsdb_app:start().
    %%application:set_env(bf_tsdb,  tsdb_root, ".eunit/db").
    %%application:set_env(bf_tsdb,  log4erl_config, "../etc/log4erl.conf").

 
stop_process(_P) ->
    application:unset_env(bf_tsdb, tsdb_root),
    bf_tsdb_app:stop(),
    ok.

run(_P) ->
    {inorder, 
     [
      ?_assertMatch(ok, bf_tsdb:create("test_db", ?SCHEMA)),
      ?_assertMatch({error, {db_already_exists, _}}, bf_tsdb:create("test_db", ?SCHEMA))
      %% ?_assertMatch("../scripts", ec_util:scripts_dir()),
      %% ?_assertMatch("../config", ec_util:config_dir()),
     ]}.


