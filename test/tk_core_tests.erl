-module(tk_core_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(LONG, 32/unsigned-little-integer).
-define(DOUBLE, 64/unsigned-little-integer).

-define(SCHEMA, [{"timestamp", {integer, 64}}, {"bid", {float, 64}}, {"ask", {float,64}}]).
-define(SCHEMA1, [{"timestamp", {integer,64}}, {"name", {float,64}}, {"bid", {float, 64}}, {"ask", {float,64}}]).
-define(TEST_TICK, {100001, 3.1345, 3.14567}).
%%-define(TEST_TICK, {calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())), 3.1345, 3.14567}).


process_test_() ->
    {setup, local,
     fun start_process/0,
     fun stop_process/1,
     fun run/1}.

start_process() ->
    tk_app:start().
    %%application:set_env(bf_tsdb,  tsdb_root, ".eunit/db").
    %%application:set_env(bf_tsdb,  log4erl_config, "../etc/log4erl.conf").

 
stop_process(_P) ->
    %application:unset_env(bf_tsdb, tk_root),
    tk_app:stop(),
    ok.

run(_P) ->
    {inorder, 
     [
      ?_assertMatch(ok, tk_core:create("test_db", ?SCHEMA)),
      ?_assertMatch({error, db_already_exists}, tk_core:create("test_db", ?SCHEMA)),
      ?_assertMatch({error, db_already_open}, tk_core:open("test_db")),
      ?_assertMatch({error,{db_not_exist, _}}, tk_core:open("test_db1")),
      %% test append / read
      ?_assertMatch([ok, ok, ok], [tk_core:append("test_db", T) || T <- [?TEST_TICK, ?TEST_TICK, ?TEST_TICK]]),
      ?_assertMatch([?TEST_TICK, ?TEST_TICK, ?TEST_TICK], tk_core:read("test_db")),
      ?_assertMatch(ok, tk_core:create("test_db1", ?SCHEMA1)),
      ?_assertMatch({error, _E}, tk_core:append("test_db1", ?TEST_TICK)),
      ?_assertMatch(ok, tk_core:close("test_db")),
      ?_assertMatch({error, db_not_open}, tk_core:append("test_db", ?TEST_TICK)),
      ?_assertMatch({error, db_not_open}, tk_core:read("test_db")),
      ?_assertMatch(ok, tk_core:close("test_db1")),
      ?_assertMatch({error, db_not_open}, tk_core:close("test_db")),
      ?_assertMatch({error, db_not_open}, tk_core:close("test_db1")),
      ?_assertMatch(ok, tk_core:open("test_db")),
      ?_assertMatch([?TEST_TICK, ?TEST_TICK, ?TEST_TICK], tk_core:read("test_db"))
     ]}.

%% test_tick() ->
%%     %%T = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
%%     B = 3.12,
%%     L = 3.15,
%%     {10, B, L}.
