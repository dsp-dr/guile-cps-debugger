# Makefile for guile-cps-debugger
# Copyright (C) 2025 dsp-dr

# Use gmake on FreeBSD for GNU Make compatibility
MAKE = gmake
GUILE = guile
GUILD = guild
prefix = /usr/local
datarootdir = $(prefix)/share
datadir = $(datarootdir)
moduledir = $(datadir)/guile/site/3.0

SOURCES = \
	cps-debugger.scm \
	cps-debugger/core.scm \
	cps-debugger/inspector.scm \
	cps-debugger/pretty.scm \
	cps-debugger/repl.scm \
	cps-debugger/analysis.scm \
	cps-debugger/compat.scm \
	cps-debugger/position.scm \
	cps-debugger/fold.scm \
	cps-debugger/stepper.scm \
	cps-debugger/session.scm \
	cps-debugger/cli.scm \
	cps-debugger/repl-commands.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

.PHONY: all compile install uninstall clean check

all: compile

compile: $(GOBJECTS)

%.go: %.scm
	GUILE_LOAD_PATH=.:$$GUILE_LOAD_PATH $(GUILD) compile -o $@ $<

install: compile
	@echo "Installing modules to $(moduledir)"
	@mkdir -p $(moduledir)/cps-debugger
	@cp cps-debugger.scm $(moduledir)/
	@cp cps-debugger/*.scm $(moduledir)/cps-debugger/
	@cp *.go $(moduledir)/
	@cp cps-debugger/*.go $(moduledir)/cps-debugger/

uninstall:
	@echo "Removing modules from $(moduledir)"
	@rm -f $(moduledir)/cps-debugger.scm
	@rm -rf $(moduledir)/cps-debugger
	@rm -f $(moduledir)/cps-debugger.go
	@rm -rf $(moduledir)/cps-debugger/*.go

clean:
	rm -f *.go cps-debugger/*.go

check: compile
	@echo "Running tests..."
	@if [ -d tests ]; then \
		for test in tests/*.scm; do \
			echo "Running $$test"; \
			$(GUILE) -L . $$test; \
		done; \
	else \
		echo "No tests found in tests/ directory"; \
	fi

# Development targets
run-repl: compile
	$(GUILE) -L . --listen

example: compile
	@echo "Running example..."
	$(GUILE) -L . -c '(use-modules (cps-debugger)) (cps-step (quote (lambda (x) (+ x 1))))'