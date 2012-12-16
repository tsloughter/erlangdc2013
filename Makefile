# -*- mode: Makefile; fill-column: 80; comment-column: 75; -*-

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=./rebar

ERLANGDC_PLT=$(CURDIR)/.depsolver_plt

.PHONY: dialyzer typer clean distclean

compile:
	@./rebar get-deps compile

rel: compile
	@./relcool

$(ERLANGDC_PLT):
	dialyzer --output_plt $(ERLANGDC_PLT) --build_plt \
		--apps erts kernel stdlib crypto public_key -r deps --fullpath

dialyzer: $(ERLANGDC_PLT)
	dialyzer --plt $(ERLANGDC_PLT) -pa deps/* --src src

typer: $(ERLANGDC_PLT)
	typer --plt $(ERLANGDC_PLT) -r ./src

clean:
	$(REBAR) clean

distclean: clean
	rm $(ERLANGDC_PLT)
	rm -rvf $(CURDIR)/deps/*
