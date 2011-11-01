-module(bf_tsdb_storage).

-export([create/2,
	 open/1,
	 close/1,
	 append/3,
	 read/1]).

-compile(export_all).

 
-type io_device() :: file:io_device().
 
%%--------------------------------------------------------------------
%% @doc
%% create a new database and head file
%% @end
%%--------------------------------------------------------------------
-spec create(string(), list()) -> {ok, io_device()} | {error, any()}.
create(FullName, Schema) ->
    case filelib:is_regular(FullName) orelse filelib:is_regular(FullName ++ ".head") of
	true -> {error, db_already_exists};
	false ->
	    %% convert schema to format used by db
	    ConvertedSchema = convert_schema(Schema),
	    %% save schema into head file
	    unconsult(FullName ++ ".head", [ConvertedSchema]),
	    %% open new db file
	    file:open(FullName, [read, append, binary, delayed_write, read_ahead])
    end.

%%--------------------------------------------------------------------
%% @doc
%% open existing db
%% @end
%%--------------------------------------------------------------------
-spec open(string()) -> {ok, io_device()} | {error, any()} | {error, {db_not_exist, string()}}.
open(FullName) ->
    %% db must already exist
    case filelib:is_regular(FullName) orelse filelib:is_regular(FullName ++ ".head") of
	true -> file:open(FullName, [read, append, binary, delayed_write, read_ahead]);
	false -> {error, {db_not_exist, FullName}}
    end.
	    
append(Record, Fd, _Schema) ->
    file:write(Fd, Record).

read(FileName) ->
    Schema = [],
    ParseFunc = get_parse_func(Schema),
    {ok, Bin} = file:read_file(FileName),
    {ok, ParseFunc(Bin)}.

close(Fd) ->
    file:close(Fd).


get_parse_func(Schema) ->
    M1 = smerl:new(tsdb_bin_parse),
    Func = expression(Schema),
    io:format("parse expression ~p ~n", [Func]),
    case smerl:add_func(M1, Func) of
	{ok, M2} ->
	    smerl:compile(M2),
	    fun(Bin) -> tsdb_bin_parse:parse_bin(Bin) end;
	{_, Other} ->
	    throw( erlang:error({err, Other}) )
    end.

expression(Schema) ->
    Keys = [string:to_upper(atom_to_list(K)) || K <- proplists:get_keys(Schema)],
    Vars = "{" ++ string:join(Keys, ",") ++ "}",
    Pre = "<<" ++ string:join([ string:to_upper(atom_to_list(Name)) ++ "/" ++ atom_to_list(Type) || {Name, Type} <- Schema], ", ") ++ ">>",
    "parse_bin(Bin) -> [ " ++ Vars ++ " || " ++ Pre ++ " <= " ++ "Bin ].". 


%%--------------------------------------------------------------------
%% @doc
%% read schema file and return schema definition
%% @end
%%--------------------------------------------------------------------
-spec read_schema(string()) -> {schema, []} | no_return.
read_schema(FileName) ->
    SchemaFile =  FileName ++ ".head",
    case file:consult(SchemaFile) of
	{ok, [Schema]} -> Schema;
	{error, _Reason} -> throw({schema_not_exist, SchemaFile})
    end.
    
 
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
-spec unconsult(string(), any()) -> ok | {error, any()}.
unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).
			   
