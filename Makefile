.PHONY: all deps compile test clean

REBAR=./rebar

all: deps compile

docs:
	@$(REBAR) doc
deps:
	@$(REBAR) get-deps
compile: deps
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
