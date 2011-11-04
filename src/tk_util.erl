%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of tickkeeper
%%%
%%% tickkeeper is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% tickkeeper is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(tk_util).

-export([
	 log4erl_config/0,
	 tk_root/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(tickkeeper, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.


%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec tk_root() -> string() | no_return().
tk_root() ->
    case application:get_env(tickkeeper, tk_root) of
	{ok, Value} -> Value;
	undefined -> throw({error, tk_root_not_defined})
    end.
