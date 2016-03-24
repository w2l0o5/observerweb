.PHONY: _build

BASE_DIR = $(shell pwd)
REBAR    = $(BASE_DIR)/rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

rel:
	@$(REBAR) release
