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

-module(tk_client).

-define(SERVER, tk_core).

%% API
-export([create/2,
	 open/1,
	 close/1,
	 append/2,
	 read/1
	]).

-spec open(string()) -> ok | {error, any()}.
open(Name) ->
    gen_server:call({global, ?SERVER}, {open, Name}).

create(Name, Schema) ->
    gen_server:call({global, ?SERVER}, {create, Name, Schema}).

close(Name) ->
    gen_server:call({global, ?SERVER}, {close, Name}).

read(Name) ->
    gen_server:call({global, ?SERVER}, {read, Name}).

append(Name, Tick) ->
    gen_server:call({global, ?SERVER}, {append, Name, Tick}).
