-module(bf_tsdb_storage).

-export([open_db/2,
	 close_db/1,
	 append/2,
	 read/3]).

-compile(export_all).

open_db(Db_Root, Name) ->
    FileName = filename:join(Db_Root, Name),
    case file:open(FileName, [read, append, binary, delayed_write, read_ahead]) of
	{ok, Fd} ->  {ok, Fd};
	{error, Reason} -> {error, Reason}
    end.

append(Record, Fd) ->
    file:write(Fd, Record).

read(Db_Root, Name, Schema) ->
    FileName = filename:join(Db_Root, Name),
    ParseFunc = get_parse_func(Schema),
    {ok, Bin} = file:read_file(FileName),
    {ok, ParseFunc(Bin)}.

close_db(Fd) ->
    file:close(Fd).


get_parse_func(Schema) ->
    M1 = smerl:new(tsdb_bin_parse),
    Func = expression(Schema),
    case smerl:add_func(M1, Func) of
	{ok, M2} ->
	    smerl:compile(M2),
	    fun(Bin) -> tsdb_bin_parse:parse_bin(Bin) end;
	{_, Other} ->
	    throw( erlang:error({err, Other}) )
    end.

expression(Schema) ->
    Keys = proplists:get_keys(Schema),
    Vars = "{" ++ string:join(Keys, ",") ++ "}",
    Pre = "<<" ++ string:join([ Name ++ "/" ++ atom_to_list(Type) || {Name, Type} <- Schema], ", ") ++ ">>",
    "parse_bin(Bin) -> [ " ++ Vars ++ " || " ++ Pre ++ " <= " ++ "Bin ].". 

    
