REBAR ?= rebar3
PROJECT := registrar

.PHONY: compile clean distclean test xref dialyzer dialyze linter lint

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

distclean: clean
	rm -rf _build

test:
	@$(REBAR) ct --cover
	@$(REBAR) cover --verbose

xref:
	@$(REBAR) as xref xref

dialyzer:
	@$(REBAR) as dialyze dialyzer

dialyze:
	@$(REBAR) as dialyze dialyzer

linter:
	@$(REBAR) as lint lint

lint:
	@$(REBAR) as lint lint
