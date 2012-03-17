all: compile

compile:
	@./rebar compile

test:
	@./rebar eunit -v

clean:
	@./rebar clean

dialyzer: compile
	@./rebar skip_deps=true dialyze

.PHONY: all compile test clean dialyzer
