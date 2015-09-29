# This Makefile is only here because I... sometimes, forgot
# the rebar commands :P

.PHONY: test

lib: test
	rebar compile

test:
	rebar compile eunit

clean:
	rebar clean

run: lib
	erl -pa ebin
