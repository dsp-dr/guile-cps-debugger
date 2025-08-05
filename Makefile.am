## Process this file with automake to produce Makefile.in

SUBDIRS = src tests docs examples

ACLOCAL_AMFLAGS = -I m4

# Extra distribution files
EXTRA_DIST = \
	README.md \
	LICENSE \
	guile.m4 \
	examples/basic-usage.scm \
	examples/advanced-analysis.scm

# Clean files
CLEANFILES = *~

# Installation directories
moddir = $(GUILE_SITE)/cps-debugger
godir = $(GUILE_SITE_CCACHE)/cps-debugger

# Guile compilation flags
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

# Default target
all-local: compile-scheme

# Compile all Scheme files
compile-scheme:
	$(AM_V_GEN)$(top_srcdir)/build-aux/compile-all.scm

# Test target
check-local:
	$(GUILE) -L $(srcdir)/src $(srcdir)/tests/run-tests.scm

# Install hook to compile files
install-data-hook:
	$(MKDIR_P) $(DESTDIR)$(godir)
	$(GUILD) compile --target="$(host)" \
		$(GUILE_WARNINGS) \
		-o "$(DESTDIR)$(godir)" \
		$(DESTDIR)$(moddir)/*.scm

# Uninstall hook
uninstall-hook:
	rm -rf $(DESTDIR)$(moddir)
	rm -rf $(DESTDIR)$(godir)

# Distribution check
distcheck-hook:
	@echo "Running distribution checks..."
	@if test -z "$(DESTDIR)"; then \
		echo "Running tests..."; \
		$(MAKE) check; \
	fi

# Development targets
.PHONY: repl test benchmark clean-go

repl:
	$(GUILE) -L $(srcdir)/src --listen

test:
	$(GUILE) -L $(srcdir)/src $(srcdir)/tests/run-tests.scm

benchmark:
	$(GUILE) -L $(srcdir)/src $(srcdir)/tests/run-tests.scm --benchmark

clean-go:
	find . -name "*.go" -delete

# Documentation generation (if needed)
.PHONY: docs

docs:
	@echo "Documentation generation not yet implemented"

# Tags generation
TAGS:
	etags --language=scheme \
		$(srcdir)/src/*.scm \
		$(srcdir)/src/*/*.scm \
		$(srcdir)/tests/*.scm \
		$(srcdir)/examples/*.scm