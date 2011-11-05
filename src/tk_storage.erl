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

-module(tk_storage).

-compile(export_all).

-export([create/2,
	 open/1,
	 close/1,
	 append/3,
	 read/2]).

-type io_device() :: file:io_device().
-type name() :: file:name(). 
%%--------------------------------------------------------------------
%% @doc
%% create a new database and head file
%% @end
%%--------------------------------------------------------------------
-spec create(name(), list()) -> {ok, io_device(), fun(), fun()} | no_return().
create(FullName, Schema) ->
    SchemaFile = FullName ++ ".head",
    case filelib:is_regular(FullName) orelse filelib:is_regular(SchemaFile) of
	true -> throw({error, db_already_exists});
	false ->
	    ConvertedSchema = convert_schema(Schema),
	    %% save schema into head file
	    unconsult(SchemaFile, [ConvertedSchema]),
	    {ok, PickleFunc, UnpickleFunc} = get_pickle_unpickle_func(ConvertedSchema),
	    case file:open(FullName, [read, append, binary, read_ahead]) of
		{ok, Fd} -> {ok, Fd, PickleFunc, UnpickleFunc}; 
		{error, Reason} -> throw({error, {cannot_open_file, FullName, Reason}})
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% open existing db
%% @end
%%--------------------------------------------------------------------
-spec open(string()) -> {ok, io_device(), fun(), fun()} | {error, any()} | {error, {db_not_exist, string()}}.
open(FullName) ->
    %% db must already exist
    case filelib:is_regular(FullName) orelse filelib:is_regular(FullName ++ ".head") of
	true ->
	    try
		ConvertedSchema = read_schema(FullName),
		{ok, PickleFunc, UnpickleFunc} = get_pickle_unpickle_func(ConvertedSchema),
		case file:open(FullName, [read, append, binary, read_ahead]) of
		    {ok, Fd} -> {ok, Fd, PickleFunc, UnpickleFunc}; 
		    {error, Reason} -> {error, Reason}
		end
	    catch
		throw:{smerl_error, Err} -> {error, Err};
		throw:{schema_not_exist, File} -> {error, {schema_not_exist, File}}
	    end;
	false -> {error, {db_not_exist, FullName}}
    end.
	    
%%--------------------------------------------------------------------
%% @doc
%% read schema file and return schema definition
%% @end
%%--------------------------------------------------------------------
-spec read_schema(string()) -> {schema, []} | no_return.
read_schema(FileName) ->
    SchemaFile = FileName ++ ".head",
    case file:consult(SchemaFile) of
	{ok, [Schema]} -> Schema;
	{error, _Reason} -> throw({schema_not_exist, SchemaFile})
    end.

 
%%--------------------------------------------------------------------
%% @doc
%% append tick to the end of the db. 
%% @end
%%--------------------------------------------------------------------
-spec append(tuple(), io_device(), fun()) -> ok | {error, any()}.
append(Data, Fd, PickleFunc) ->
    try
	Bin = PickleFunc(Data),
	case file:write(Fd, Bin) of
	    ok -> ok;
	    {error, Err} -> {error, {can_not_save_tick_to_db, Err}}
	end
    catch
	_:Reason -> {error, {can_not_convert_tick_to_binary, "make sure tick matches db schema", Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% read entire curve from db.
%% @end
%%--------------------------------------------------------------------
-spec read(string(), fun()) -> {ok, []} | {error, {cant_read_from_db, string(), any()}}.
read(FileName, UnpickleFunc) ->
    try
	{ok, Bin} = file:read_file(FileName),
	{ok, UnpickleFunc(Bin)}
    catch
	_:Reason -> {error, {cant_read_from_db, FileName, Reason}}
    end.
	    
%%--------------------------------------------------------------------
%% @doc
%% close open db
%% @end
%%--------------------------------------------------------------------
-spec close(io_device()) -> ok | {error, any()}.
close(Fd) ->
    file:close(Fd).
 
%%--------------------------------------------------------------------
%% @doc
%% create pickle/unpickle functions for a given schema
%% @end
%%--------------------------------------------------------------------
-spec get_pickle_unpickle_func({schema, []}) -> {ok, fun(), fun()} | no_return().
get_pickle_unpickle_func(Schema) ->
    M1 = smerl:new(tsdb_pickle_fns),
    PickleFunc = pickle_expression(Schema),
    UnpickleFunc = unpickle_expression(Schema),
    %log4erl:debug("parse expressions: ~p : ~p", [PickleFunc, UnpickleFunc]),
    {ok, M2} = compile_func(M1, PickleFunc),
    {ok, _M3} = compile_func(M2, UnpickleFunc),
    {ok, fun(Bin) -> tsdb_pickle_fns:pickle(Bin) end, fun(Bin) -> tsdb_pickle_fns:unpickle(Bin) end}.

%%--------------------------------------------------------------------
%% @doc
%% compile function expression 
%% @end
%%--------------------------------------------------------------------
-spec compile_func(tuple(), string()) -> {ok, tuple()} | no_return().
compile_func(M, Expr) ->
    case smerl:add_func(M, Expr) of
	{ok, M2} -> 
	    case smerl:compile(M2) of
		ok -> {ok, M2};
		{error, Err} -> throw({smerl_error, Err})
	    end;
	{_, Other} -> throw({smerl_error, Other})
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% build string representation of pickle expression.
%% This expression will be compiled into a function
%% used to save tick in the given schema into binary format.
%% @end
%%--------------------------------------------------------------------
-spec pickle_expression({schema, []}) -> string().
pickle_expression({schema, Fields}) ->
    %% make sure fields are in order
    SortedFields = lists:sort(fun({field, {id, A}, _N, _T}, {field, {id, B}, _N1, _T1}) -> A =< B end, Fields),
    Name_Type = [{Name, Type} || {field, _Id, {name, Name}, {type, Type}} <- SortedFields],
    Keys = [string:to_upper(K) || {K,_T} <- Name_Type],
    Vars = "{" ++ string:join(Keys, ",") ++ "}",
    Pre = "<<" ++ string:join([string:to_upper(Name) ++ ":" ++ integer_to_list(Size) ++ "/" ++ atom_to_list(Type1) ||
				  {Name, {Type1, Size}} <- Name_Type], ", ") ++ ">>",
    "pickle(" ++ Vars ++ ") -> " ++ Pre ++ ".".

%%--------------------------------------------------------------------
%% @doc
%% build string representation of unpickle expression. 
%% This expression will be compiled into a function
%% used to parse stream of data rerieved from binary format of db.
%% @end
%%--------------------------------------------------------------------
-spec unpickle_expression({schema, []}) -> string().
unpickle_expression({schema, Fields}) ->
    %% make sure fields are in order
    SortedFields = lists:sort(fun({field, {id, A}, _N, _T}, {field, {id, B}, _N1, _T1}) -> A =< B end, Fields),
    Name_Type = [{Name, Type} || {field, _Id, {name, Name}, {type, Type}} <- SortedFields],
    Keys = [string:to_upper(K) || {K,_T} <- Name_Type],
    Vars = "{" ++ string:join(Keys, ",") ++ "}",
    Pre = "<<" ++ string:join([string:to_upper(Name) ++ ":" ++ integer_to_list(Size) ++ "/" ++ atom_to_list(Type1)
			       || {Name, {Type1, Size}} <- Name_Type], ", ") ++ ">>",
    "unpickle(Bin) -> [ " ++ Vars ++ " || " ++ Pre ++ " <= " ++ "Bin ].".
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% convert schema from user defined , e.g. [{timestamp, integer}, {bid, float}, {ask, float}]
%% to db schema .e.g.
%% {schema,[{field,{id,0},{name,"timestamp"},{type,integer}},
%%          {field,{id,1},{name,"bid"},{type,float}},
%%          {field,{id,2},{name,"ask"},{type,float}}]}.
%% @end
%%--------------------------------------------------------------------
-spec convert_schema(list()) -> {schema, list()}.
convert_schema(Schema) ->
    convert_schema(Schema, 0, []).
convert_schema([], _Count, Acc) ->
    {schema, lists:reverse(Acc)};
convert_schema([{Name, Type} | T], Count, Acc) ->	     
    convert_schema(T, Count + 1, [{field, {id, Count}, {name, Name}, {type, Type}} | Acc]).
    
 
%%--------------------------------------------------------------------
%% @doc
%% save erlang terms into file. from Programming Erlang book p.228
%% @end
%%--------------------------------------------------------------------
-spec unconsult(name(), any()) -> ok | no_return().
unconsult(FileName, L) ->
    try
	{ok, S} = file:open(FileName, [write]),
	lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
	ok = file:close(S)
    catch
	_:_ -> throw({error, cant_write_to_file, FileName})
    end.
	

