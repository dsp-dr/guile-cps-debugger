## Process this file with automake to produce Makefile.in

# Installation directories
moddir = $(GUILE_SITE)/cps-debugger
coredir = $(moddir)/core
analysisdir = $(moddir)/analysis
uidir = $(moddir)/ui

godir = $(GUILE_SITE_CCACHE)/cps-debugger
gocore = $(godir)/core
goanalysis = $(godir)/analysis
goui = $(godir)/ui

# Source files
SOURCES = \
	config.scm \
	core/debugger.scm \
	analysis/continuation.scm \
	analysis/memory.scm \
	ui/repl-commands.scm \
	ui/visualizer.scm

# Installation rules
dist_mod_DATA = config.scm

dist_core_DATA = \
	core/debugger.scm

dist_analysis_DATA = \
	analysis/continuation.scm \
	analysis/memory.scm

dist_ui_DATA = \
	ui/repl-commands.scm \
	ui/visualizer.scm

# Guile compilation
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(MKDIR_P) `dirname "$@"`
	$(AM_V_at)$(GUILD) compile $(GUILE_WARNINGS) \
		-L "$(top_srcdir)/src" \
		-o "$@" "$<"

# Compiled files
go_DATA = config.go
gocore_DATA = core/debugger.go
goanalysis_DATA = analysis/continuation.go analysis/memory.go
goui_DATA = ui/repl-commands.go ui/visualizer.go

CLEANFILES = $(go_DATA) $(gocore_DATA) $(goanalysis_DATA) $(goui_DATA)

# Extra distribution files
EXTRA_DIST = $(SOURCES)