## Process this file with automake to produce Makefile.in

# Documentation directories
docdir = $(datadir)/doc/$(PACKAGE)
apidir = $(docdir)/api
userdir = $(docdir)/user
developerdir = $(docdir)/developer

# Documentation files
dist_doc_DATA = \
	README.md

# API documentation
dist_api_DATA = \
	api/core-api.md \
	api/repl-api.md

# User documentation  
dist_user_DATA = \
	user/getting-started.md \
	user/tutorial.md

# Developer documentation
dist_developer_DATA = \
	developer/architecture.md \
	developer/contributing.md

# Extra files
EXTRA_DIST = \
	$(dist_doc_DATA) \
	$(dist_api_DATA) \
	$(dist_user_DATA) \
	$(dist_developer_DATA)

# Documentation generation targets
.PHONY: html pdf

html:
	@echo "HTML documentation generation not yet implemented"

pdf:
	@echo "PDF documentation generation not yet implemented"

# Clean generated documentation
clean-local:
	rm -rf html/ pdf/