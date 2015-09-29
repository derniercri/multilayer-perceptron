# This Makefile is only here because I... sometimes, forgot
# the rebar commands :P

.PHONY: test

all: 
	rebar compile

test:
	rebar compile eunit

clean:
	rebar clean

run: 
	erl -pa ebin -pa ./deps/*/ebin
