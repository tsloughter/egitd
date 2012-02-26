.PHONY: compile clean

all:
	@./rebar update-deps
	@./rebar get-deps
	@./rebar compile

compile:
	@./rebar compile

clean:
	@./rebar clean

