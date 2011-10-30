-module(bf_tsdb_storage).

-export([open_db/2,
	 close_db/1,
	 append/2,
	 read/2]).

%%-compile(export_all).

open_db(Db_Root, Name) ->
    FileName = filename:join(Db_Root, Name),
    case file:open(FileName, [read, append, binary, delayed_write, read_ahead]) of
	{ok, Fd} ->  {ok, Fd};
	{error, Reason} -> {error, Reason}
    end.

append(Record, Fd) ->
    file:write(Fd, Record).

read(Db_Root, Name) ->
    FileName = filename:join(Db_Root, Name),
    {ok, Bin} = file:read_file(FileName),
    {ok, [{calendar:gregorian_seconds_to_datetime(T), B, L} || <<T/integer, B/float, L/float>> <= Bin]}.

close_db(Fd) ->
    file:close(Fd).
