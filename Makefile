all: get-deps compile

get-deps:
	rebar get-deps

compile:
	mkdir -p db
	rebar compile
clean:
	rebar clean
	rm -rf test/*.beam

test: all
	mkdir -p .eunit
	mkdir -p .eunit/db
	cp -r etc .eunit/.
	rm -rf .eunit/db/*
	rebar skip_deps=true eunit

doc:
	rebar doc skip_deps=true

rel: all
	rebar generate
	chmod u+x rel/tickkeeper/bin/tickkeeper

relclean:
	rm -rf rel/tickkeeper

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
xmerl snmp public_key mnesia eunit syntax_tools compiler hipe webtool
COMBO_PLT = $(HOME)/.tk_dialyzer_plt

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
