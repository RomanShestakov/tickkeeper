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

-module(tk_core).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 create/2,
	 open/1,
	 close/1,
	 append/2,
	 read/1]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {tk_root, open_db = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
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

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initializes the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    TK_Root = tk_util:tk_root(),
    log4erl:info("tickkeeper root: ~p", [TK_Root]),    
    {ok, #state{tk_root = TK_Root}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create, Name, Schema}, _From, State) ->
    case tk_storage:create(db_full_name(State#state.tk_root, Name), Schema) of
	{ok, Fd, PickleFunc, UnpickleFunc} ->
	    {reply, ok, State#state{open_db = [{Name, {Fd, PickleFunc, UnpickleFunc}} | State#state.open_db]}};
	{error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({open, Name}, _From, State) ->
    case proplists:lookup(Name, State#state.open_db) of
	{Name, _Fd} -> {reply, {error, db_already_open}, State};
	none ->
	    case tk_storage:open(db_full_name(State#state.tk_root, Name)) of
		{ok, Fd, PickleFunc, UnpickleFunc} ->
		    {reply, ok, State#state{open_db = [{Name, {Fd, PickleFunc, UnpickleFunc}} | State#state.open_db]}};
		{error, Reason} -> {reply, {error, Reason}, State}
	    end
    end;
handle_call({close, Name}, _From, State) ->
    case proplists:lookup(Name, State#state.open_db) of
	{Name, {Fd, _PickleFunc, _UnpickleFunc}} -> 
	    Result = tk_storage:close(Fd),
	    {reply, Result, State#state{open_db = proplists:delete(Name, State#state.open_db)}};
	none ->
	    {reply, {error, db_not_open}, State}
    end;
handle_call({append, Name, Data}, _From, State) ->
    case proplists:lookup(Name, State#state.open_db) of
	{Name, {Fd, PickleFunc, _UnpickleFunc}} -> 
	    case tk_storage:append(Data, Fd, PickleFunc) of
		ok -> {reply, ok, State};
		{error, Err} -> {reply, {error, Err}, State}
	    end;
	none -> 
 	    log4erl:error("cant' add tick as db ~p is not open", [Name]),
	    {reply, {error, db_not_open}, State}
    end;
handle_call({read, Name}, _From, State) ->
    FullName = db_full_name(State#state.tk_root, Name),
    case proplists:lookup(Name, State#state.open_db) of
	{Name, {_Fd, _PickleFunc, UnpickleFunc}} -> 
    	    case tk_storage:read(FullName, UnpickleFunc) of
		{ok, Curve} -> {reply, Curve, State};
		{error, Reason} -> {reply, {error, Reason}, State}
	    end;
	none -> {reply, {error, db_not_open}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% handle_cast({append, Name, Data}, State) ->
%%     case proplists:lookup(Name, State#state.open_db) of
%% 	{Name, {Fd, PickleFunc, _UnpickleFunc}} -> 
%% 	    case bf_tsdb_storage:append(Data, Fd, PickleFunc) of
%% 		ok -> ok;
%% 		{error, Err} -> log4erl:error("can't add tick ~p, ~p", [Data, Err])
%% 	    end,
%% 	    {noreply, State};    
%% 	none -> 
%%  	    log4erl:error("cant' add tick as db ~p is not open", [Name]),    
%% 	    {noreply, State}
%%     end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    [tk_storage:close(Fd) || {_Name, Fd} <- State#state.open_db],
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
db_full_name(Db_Root, Name) ->
    filename:join(Db_Root, Name).

