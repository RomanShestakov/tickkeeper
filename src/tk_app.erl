%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of tickkeeper
%%%
%%% bf_bot is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% bf_bot is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(tk_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-define(APPS, [log4erl, tickkeeper]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% to start manually from console with start.sh
start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    Config = tk_util:log4erl_config(),
    log4erl:conf(Config),
    log4erl:info("starting tickkeeper"),
    tk_sup:start_link().

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping tickkeeper"),
    [application:stop(A) || A <- lists:reverse(?APPS)].
