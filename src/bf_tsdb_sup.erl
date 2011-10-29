
-module(bf_tsdb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% %% Helper macro for declaring children of supervisor
%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Bf_TSDB = {'bf_tsdb',{'bf_tsdb',start_link,[]},permanent,2000,worker,['bf_tsdb']},
    {ok, {{one_for_one, 5, 10}, [Bf_TSDB]}}.
