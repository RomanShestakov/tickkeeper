-module(bf_tsdb_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(LONG, 32/unsigned-little-integer).
-define(DOUBLE, 64/unsigned-little-integer).

-define(SCHEMA, [{"timestamp", 64}, {"bid", 64}, {"ask", 64}]).
-define(SCHEMA1, [{"timestamp", integer}, {"name", integer}, {"bid", float}, {"ask", float}]).

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
      ?_assertMatch({error, db_already_exists}, bf_tsdb:create("test_db", ?SCHEMA)),
      ?_assertMatch({error, db_already_open}, bf_tsdb:open("test_db")),
      ?_assertMatch({error,{db_not_exist, _}}, bf_tsdb:open("test_db1")),
      %% test append / read
      ?_assertMatch(ok, bf_tsdb:append("test_db", test_tick())),
      %%?_assertMatch({ok, _C}, bf_tsdb:read("test_db")),
      ?_assertMatch(ok, bf_tsdb:create("test_db1", ?SCHEMA1)),
      ?_assertMatch(ok, bf_tsdb:close("test_db")),
      ?_assertMatch(ok, bf_tsdb:close("test_db1")),
      ?_assertMatch({error, db_not_open}, bf_tsdb:close("test_db")),
      ?_assertMatch({error, db_not_open}, bf_tsdb:close("test_db1"))
     ]}.

test_tick() ->
    %%T = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    B = 3.12,
    L = 3.15,
    {10, B, L}.

