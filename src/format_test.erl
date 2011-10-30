-module(format_test).
-compile(export_all).

open(Name) ->
    open("/Users/romanshestakov/temp", Name).
     
open(Db_Root, Name) ->
    FileName = filename:join(Db_Root, Name),
    case file:open(FileName, [read, append, binary, delayed_write, read_ahead]) of
	{ok, Fd} ->  {ok, Fd};
	{error, Reason} -> {error, Reason}
    end.
