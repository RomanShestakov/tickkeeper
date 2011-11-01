all: get-deps compile

get-deps:
	rebar get-deps

compile:
	rebar compile
clean:
	rebar clean
	rm -rf test/*.beam

test: all
	cp -r etc .eunit/.
	rm -rf db/test_db*
	rebar skip_deps=true eunit

doc:
	rebar doc skip_deps=true

rel: all
	rebar generate
	chmod u+x rel/bf_tsdb/bin/bf_tsdb

relclean:
	rm -rf rel/bf_tsdb

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
xmerl snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.bf_tsdb_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin | fgrep -v -f ./dialyzer.ignore-warning

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
