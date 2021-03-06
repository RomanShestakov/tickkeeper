tickkeeper - simple timeseries database
==============================================

needed a simple way to store time series data and it couldn't find anything available in open source.
This is really simple implementation done in Erlang using binary files as storage. Allows to define custom schema's for ticks.
No idea at this point how performant or scalable this thing is. Also haven't done anything to allow range selects, so read call retrieves entire curve. There is certantly a room for improvements (maybe REST API, connections over TCP sockets, bindings to other languages, etc).


## Dependencies

1. log4erl for logging
2. Smerl for creating/compiling schemas dynamically

## Building

tickkeeper uses rebar for building and wraps it in a Makefile for convenience, so clone / build rebar from here:
https://github.com/basho/rebar

Then clone tickkeeper from GitHub:

    $ git clone git@github.com:RomanShestakov/tickkeeper.git

Then change into the newly created directory:

    $ cd tickkeeper

run make to build / test:

    $ make test

then build release:

    $ make rel

start app with 

./start.sh

or start release with:
./rel/tickkeeper/bin/tickkeeper console

create test db:
tk_client:create("test_db",  [{"timestamp", {integer, 64}}, {"bid", {float, 64}}]).

save tick:
tk_client:append("test_db", {calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())), 3.1345}).

read curve from db:
tk_client:read("test_db").

License
=======
