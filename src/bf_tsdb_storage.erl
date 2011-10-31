-module(bf_tsdb_storage).

-export([create/2,
	 open/1,
	 close/1,
	 append/3,
	 read/1]).

-compile(export_all).


create(FullName, Schema) ->
    case filelib:is_regular(FullName) of
	true -> {error, {db_already_exists, FullName}};
	false ->
	    %% convert schema to format used by db
	    ConvertedSchema = convert_schema(Schema),
	    %% same schema into head file
	    unconsult(FullName ++ ".head", [ConvertedSchema]),
	    %% open db
	    open(FullName)
    end.
	    
open(FileName) ->
    %%%Schema = read_schema(FileName).
    case file:open(FileName, [read, append, binary, delayed_write, read_ahead]) of
	{ok, Fd} ->  {ok, Fd};
	{error, Reason} -> {error, Reason}
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


%%
%% return schema
%%
-spec read_schema(string()) -> {schema, []} | no_return.
read_schema(FileName) ->
    SchemaFile =  FileName ++ ".head",
    case file:consult(SchemaFile) of
	{ok, [Schema]} -> Schema;
	{error, _Reason} -> throw({schema_not_exist, SchemaFile})
    end.
    

%%Schema = [{timestamp, integer}, {bid, float}, {ask, float}],
convert_schema(Schema) ->
    convert_schema(Schema, 0, []).
convert_schema([], _Count, Acc) ->
    {schema, lists:reverse(Acc)};
convert_schema([{Name, Type} | T], Count, Acc) ->	     
    convert_schema(T, Count + 1, [{field, {id, Count}, {name, Name}, {type, Type}} | Acc]).
    

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).


			   
