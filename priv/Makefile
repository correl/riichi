.PHONY: all node-deps clean run

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

TARGET = js/riichi.js
SOURCE = src/Riichi.elm
CSS = css/riichi.css
CSS_SOURCE = src/Stylesheets.elm

ELM_FILES = $(shell find . -type f -name '*.elm')
NODE_PATH := $(abspath ./node_modules/.bin)
ELM-MAKE := $(NODE_PATH)/elm-make
ELM-REACTOR := $(NODE_PATH)/elm-reactor
ELM-TEST := $(NODE_PATH)/elm-test
ELM-CSS := $(NODE_PATH)/elm-css

ELMMAKE_FLAGS = --yes --warn

ifeq ($(DEBUG),1)
	ELMMAKE_FLAGS += --debug
endif

all: node-deps $(TARGET) $(CSS)

node-deps:
	npm i

$(TARGET): $(ELM_FILES)
	$(ELM-MAKE) $(ELMMAKE_FLAGS) src/Riichi.elm --output=$@

$(CSS): $(CSS_SOURCE)
	@PATH=$(NODE_PATH):$(PATH) $(ELM-CSS) $(CSS_SOURCE) -o css

clean-deps:
	rm -rf elm-stuff
	rm -rf node_modules

test:
	@PATH=$(NODE_PATH):$(PATH) $(ELM-TEST)

clean:
	rm -f $(TARGET) $(CSS)
	rm -rf elm-stuff/build-artifacts

run: all
	$(ELM-REACTOR)

