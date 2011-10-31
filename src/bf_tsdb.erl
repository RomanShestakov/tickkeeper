%%%-------------------------------------------------------------------
%%% File    : bf_tsdb.erl
%%% Author  : Roman Shestakov <>
%%% Description : 
%%%
%%% Created : 29 Oct 2011 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(bf_tsdb).

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

-record(state, {tsdb_root, open_db = []}).

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

append(Tick, Name) ->
    gen_server:cast({global, ?SERVER}, {append, Tick, Name}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initializes the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    TSDB_Root = bf_tsdb_util:tsdb_root(),
    log4erl:info("tsdb root: ~p", [TSDB_Root]),    
    {ok, #state{tsdb_root = TSDB_Root}}.

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
    case bf_tsdb_storage:create(db_full_name(State#state.tsdb_root, Name), Schema) of
	{ok, Fd} ->
	    {reply, ok, State#state{open_db = [{Name, Fd} | State#state.open_db]}};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({open, Name}, _From, State) ->
    case bf_tsdb_storage:open(db_full_name(State#state.tsdb_root, Name)) of
	{ok, Fd} ->
	    {reply, ok, State#state{open_db = [{Name, Fd} | State#state.open_db]}};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;
handle_call({close, Name}, _From, State) ->
    case proplists:lookup(Name, State#state.open_db) of
	{Name, Fd} -> 
	    Result = bf_tsdb_storage:close(Fd),
	    {reply, Result, State#state{open_db = proplists:delete(Name, State#state.open_db)}};
	none ->
	    log4erl:error("db ~p is not open", [Name]),    
	    {reply, error_db_not_open, State}
    end;
handle_call({read, Name}, _From, State) ->
    %%Schema = [{timestamp, integer}, {bid, float}, {ask, float}],
    case bf_tsdb_storage:read(db_full_name(State#state.tsdb_root, Name)) of
	{ok, Curve} -> 
	    {reply, Curve, State};
	{error, Reason} ->
	    log4erl:error("cant' read db: ~p, ~p", [Name, Reason]),    
	    {reply, {error, Reason}, State}
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
handle_cast({append, Data, Name}, State) ->
    case proplists:lookup(Name, State#state.open_db) of
	{Name, Fd} -> 
	    bf_tsdb_storage:append(Data, Fd, [{timestamp, integer}, {bid, float}, {ask, float}]),
	    {noreply, State};    
	none -> 
 	    log4erl:error("cant' add tick as db ~p is not open", [Name]),    
	    {noreply, State}
    end;
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
    [ bf_tsdb_storage:close(Fd) || {_Name, Fd} <- State#state.open_db],
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
