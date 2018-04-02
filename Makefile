.PHONY: all elm elm-test riichi riichi-test test clean

all: elm riichi

elm:
	$(MAKE) $(MAKE_FLAGS) --directory priv

elm-test:


riichi:
	rebar3 compile

elm-test:
	$(MAKE) test $(MAKE_FLAGS) --directory priv

test: elm-test
	rebar3 eunit

clean:
	$(MAKE) clean $(MAKE_FLAGS) --directory priv
