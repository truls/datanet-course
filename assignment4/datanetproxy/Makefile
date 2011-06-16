# Makefile for datanetproxy

REBAR=./rebar

all: deps compile rel
	@echo
	@echo
	@echo "All done. Now start the proxy by running rel/datanetproxy/bin/datanetproxy start"
	@echo "You can configure the proxy by editing rel/datanetproxy/etc/app.config"
	@echo
	@echo "Have fun!"

deps:
	$(REBAR) get-deps
	grep -q DATANET deps/ibrowse/src/ibrowse_http_client.erl || < datanet-request-method.patch patch -d deps/ibrowse/src/ -p0

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
