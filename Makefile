all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

dialyzer: compile
	@./rebar skip_deps=true dialyze

.PHONY: all compile clean dialyzer
