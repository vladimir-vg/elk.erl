REBAR = ./rebar

.PHONY: all compile test clean

all: compile

compile:
	@$(REBAR) compile

test_deps:
	@$(REBAR) --config test.config get-deps

test_compile:
	@$(REBAR) --config test.config compile

test: test_deps test_compile
	@$(REBAR) --config test.config eunit

clean:
	@$(REBAR) clean
