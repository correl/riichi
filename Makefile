.PHONY: all deps compile test clean

REBAR=rebar
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=kernel stdlib erts mnesia eunit

all: deps compile

docs:
	@$(REBAR) doc
deps:
	@$(REBAR) get-deps
	@$(REBAR) update-deps
compile: deps
	@$(REBAR) compile

$(DEPS_PLT):
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
		--apps $(DEPS) -r deps
dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r ./ebin
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
	@$(REBAR) delete-deps
