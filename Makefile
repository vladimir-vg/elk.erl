REBAR = ./rebar

.PHONY: all compile test clean

all: compile

compile:
	@$(REBAR) compile

test_deps:
	@$(REBAR) get-deps

test_compile:
	@$(REBAR) compile

test: test_deps test_compile
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
