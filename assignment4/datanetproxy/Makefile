# Makefile for datanetproxy

REBAR=./rebar

all: deps compile rel

deps:
	$(REBAR) get-deps

cleandeps:
	$(REBAR) delete-deps

compile:
	$(REBAR) compile

rel: compile rel/datanetproxy

rel/datanetproxy:
	$(REBAR) generate

clean:
	$(REBAR) clean

relclean:
	rm -rf rel/datanetproxy

distclean: clean relclean cleandeps

devconsole:
	rel/datanetproxy/bin/datanetproxy console \
		-pa ../../apps/proxy/ebin

.PHONY: all compile deps
