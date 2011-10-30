-module(tsdb_test).
-compile(export_all).

-define(LONG, 32/unsigned-little-integer).
-define(DOUBLE, 64/unsigned-little-integer).

-define(ROOT, "/Users/romanshestakov/tmp" ).


tick() ->
    T = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())),
    B = 3.15,
    L = 3.13,
    <<T/integer, B/float, L/float>>.
    

open(Name) ->
    open(?ROOT, Name).
     
open(Db_Root, Name) ->
    FileName = filename:join(Db_Root, Name),
    case file:open(FileName, [read, append, binary, delayed_write, read_ahead]) of
	{ok, Fd} ->  {ok, Fd};
	{error, Reason} -> {error, Reason}
    end.

%% write(Tick, Dev, Schema) ->
%%     file:write(Dev, 

read(Name) ->
    %%{ok, Dev} = open(Name),
    FileName = filename:join(?ROOT, Name),
    {ok, Bin} = file:read_file(FileName),
    [{calendar:gregorian_seconds_to_datetime(T), B, L} || <<T/integer, B/float, L/float>> <= Bin].
