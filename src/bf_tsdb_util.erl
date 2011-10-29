%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of bf_bot
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

-module(bf_tsdb_util).

-export([
	 log4erl_config/0,
	 tsdb_root/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(bf_tsdb, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.


%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec tsdb_root() -> string() | no_return().
tsdb_root() ->
    case application:get_env(bf_tsdb, tsdb_root) of
	{ok, Value} -> Value;
	undefined -> throw({error, tsdb_root_not_defined})
    end.
