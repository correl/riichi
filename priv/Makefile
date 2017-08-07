.PHONY: all node-deps clean run

TARGET = js/riichi.js
SOURCE = src/Riichi.elm
CSS = css/riichi.css
CSS_SOURCE = src/Stylesheets.elm

ELM_FILES = $(shell find . -type f -name '*.elm')
PATH := ./node_modules/.bin:$(PATH)

ELMMAKE_FLAGS = --yes --warn

ifeq ($(DEBUG),1)
	ELMMAKE_FLAGS += --debug
endif

all: node-deps $(TARGET) $(CSS)

node-deps:
	npm i

$(TARGET): $(ELM_FILES)
	elm-make $(ELMMAKE_FLAGS) src/Riichi.elm --output=$@

$(CSS): $(CSS_SOURCE)
	elm-css $(CSS_SOURCE) -o css

clean-deps:
	rm -rf elm-stuff
	rm -rf node_modules

clean:
	rm -f $(TARGET) $(CSS)
	rm -rf elm-stuff/build-artifacts

run: all
	elm-reactor
