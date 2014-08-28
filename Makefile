PROJECT = riichi

DEPS = reloader eunit_formatters
dep_reloader = git https://github.com/oinksoft/reloader.git master
dep_eunit_formatters = git https://github.com/seancribbs/eunit_formatters.git master

include erlang.mk

ifneq ($(wildcard test/),)
ebin/$(PROJECT).app:: $(shell find test -type f -name \*_tests.erl)
	$(if $(strip $?),$(call compile_erl,$?))
endif

eunit: clean deps app
	$(gen_verbose) erl -noshell -pa ebin -eval 'eunit:test({application, $(PROJECT)}, [no_tty, {report, {eunit_progress, [colored, profile]}}])' -s init stop
