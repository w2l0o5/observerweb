.PHONY: rel deps

all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

rel: deps
	cd rel && rebar compile generate
