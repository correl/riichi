all: elm

elm:
	$(MAKE) $(MAKE_FLAGS) --directory priv

clean:
	$(MAKE) clean $(MAKE_FLAGS) --directory priv
