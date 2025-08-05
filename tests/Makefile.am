## Process this file with automake to produce Makefile.in

# Test files
TESTS = \
	test-framework.scm \
	run-tests.scm

# Extra test files
EXTRA_DIST = $(TESTS)

# Test environment
TESTS_ENVIRONMENT = \
	GUILE_LOAD_PATH="$(top_srcdir)/src:$$GUILE_LOAD_PATH" \
	$(GUILE) -L $(top_srcdir)/src

# Check target
check-local:
	@echo "Running CPS debugger tests..."
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm

# Individual test targets
.PHONY: test-core test-memory test-visualization test-integration

test-core:
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm -s core

test-memory:
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm -s memory

test-visualization:
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm -s visualization

test-integration:
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm -s integration

# Benchmark target
.PHONY: benchmark

benchmark:
	@echo "Running performance benchmarks..."
	@$(TESTS_ENVIRONMENT) $(srcdir)/run-tests.scm -b

# Clean target
clean-local:
	rm -f *.log *.trs