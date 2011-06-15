# Makefile for datanet proxyserven

#.SUFFIXES=.erl .beam

EBIN=ebin
ESRC=src

ERLC_FLAGS = +debug_info -o ebin
ERLC = erlc $(ERLC_FLAGS)


#all: compile

compile: pr_server.beam pr_tracker.beam pr_app.beam pr_sup.beam pr_root_sup.beam pr_nodeparser.beam

%.beam: src/%.erl
#@echo $^
	$(ERLC) $<

clean:
	rm -rf $(EBIN)/*.beam
