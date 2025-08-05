# Changelog

All notable changes to the Guile CPS Debugger will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-08-05

### Added
- Initial project structure and build system
- Core debugging engine with basic CPS inspection
- Pretty-printing functionality for CPS and pseudo-CPS terms
- Compatibility layer supporting both Guile 2.x and 3.x
  - Fallback to Tree-IL and pseudo-CPS for Guile 2.x
  - Full CPS support for Guile 3.x
- Inspector module for analyzing CPS structure
- Static analysis tools:
  - Node counting
  - Call site detection
  - Variable usage analysis
  - Free variable computation
- REPL integration with custom commands
- Comprehensive test suite with 5 test modules
- Working examples demonstrating:
  - Lambda inspection
  - Pretty-printing
  - Static analysis
- Demo infrastructure:
  - Asciinema recordings
  - GIF animations
  - Tmux session setup
- Emacs integration with Geiser configuration
- Documentation:
  - Comprehensive README.org
  - API documentation in source files
  - Demo documentation

### Known Issues
- Full CPS functionality requires Guile 3.0+
- On Guile 2.x, uses pseudo-CPS representation derived from Tree-IL

### Contributors
- dsp-dr
- Co-authored by Claude (AI assistant)

[0.1.0]: https://github.com/dsp-dr/guile-cps-debugger/releases/tag/v0.1.0