## Process this file with automake to produce Makefile.in

# Example files
dist_examples_DATA = \
	basic-usage.scm \
	advanced-analysis.scm

# Installation directory
examplesdir = $(datadir)/examples/$(PACKAGE)

# Extra distribution files
EXTRA_DIST = $(dist_examples_DATA)

# Make examples executable on install
install-data-hook:
	chmod +x $(DESTDIR)$(examplesdir)/*.scm

# Run examples
.PHONY: run-basic run-advanced

run-basic:
	$(GUILE) -L $(top_srcdir)/src $(srcdir)/basic-usage.scm

run-advanced:
	$(GUILE) -L $(top_srcdir)/src $(srcdir)/advanced-analysis.scm