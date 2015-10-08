.PHONY: rel deps

all: deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

rel: deps
	rebar compile && cd rel && rebar generate
