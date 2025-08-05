# Guile CPS Debugger

A comprehensive debugging and analysis framework for Continuation-Passing Style (CPS) code in Guile Scheme.

## Overview

The Guile CPS Debugger provides advanced debugging capabilities, memory profiling, and visualization tools specifically tailored for understanding CPS transformation and execution. It integrates seamlessly with Guile's REPL and provides both programmatic and interactive interfaces.

## Features

- **CPS-aware tracing**: Track continuation chains with clear visualization
- **Memory profiling**: Analyze closure allocation and detect memory leaks
- **REPL integration**: Interactive debugging commands
- **Visualization**: ASCII and Graphviz output for continuation chains
- **Performance analysis**: Benchmark different CPS strategies
- **Configurable**: Extensive configuration options

## Requirements

- Guile 3.0 or later
- GNU Autotools (for building from source)
- Graphviz (optional, for visualization)

## Installation

### From Source

```bash
cd guile-cps-debugger
autoreconf -vif
./configure
make
make check
sudo make install
```

### Configuration Options

```bash
./configure --enable-visualization    # Enable visualization support (default: yes)
./configure --enable-memory-profiling # Enable memory profiling (default: yes)
./configure --prefix=/usr/local       # Installation prefix
```

## Quick Start

### Basic Usage

```scheme
(use-modules (cps-debugger core debugger))

;; Create a debugger instance
(define dbg (make-cps-debugger))

;; Define a CPS function
(define (factorial-cps n k)
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (r) (k (* n r))))))

;; Trace execution
(debugger-trace-cps dbg 
  (lambda () (factorial-cps 5 (lambda (x) x))))
```

### REPL Integration

```scheme
(use-modules (cps-debugger ui repl-commands))

;; Install REPL commands
(install-cps-commands (current-repl))

;; Use debugging commands
,cps-trace (factorial-cps 5 identity)
,cps-profile (fibonacci-cps 10 identity)
,cps-visualize ascii
,cps-config max-trace-depth 200
```

## REPL Commands

- `,cps-trace EXPR` - Trace CPS execution
- `,cps-profile EXPR` - Profile memory usage  
- `,cps-break PROC` - Set breakpoint (planned)
- `,cps-chain` - Show continuation chain
- `,cps-memory` - Show memory statistics
- `,cps-visualize [FORMAT]` - Visualize chain (ascii/dot)
- `,cps-config [KEY] [VALUE]` - Configure debugger
- `,cps-clear` - Clear debugger state
- `,cps-help` - Show help

## Examples

See the `examples/` directory for detailed examples:

- `basic-usage.scm` - Simple CPS debugging examples
- `advanced-analysis.scm` - Complex analysis scenarios

Run examples:
```bash
./examples/basic-usage.scm
./examples/advanced-analysis.scm
```

## API Documentation

### Core API

```scheme
;; Create debugger
(make-cps-debugger [#:vm vm] [#:options options])

;; Trace CPS execution
(debugger-trace-cps debugger thunk)
;; => values: result error state

;; Profile memory
(debugger-profile-memory debugger thunk)  
;; => values: result report

;; Configuration
(debugger-set-option! debugger key value)
(debugger-get-option debugger key)
```

### Analysis API

```scheme
;; Analyze continuation chain
(analyze-continuation-chain trace-data)
;; => <continuation-info>

;; Build continuation tree
(build-continuation-tree trace-data)
;; => <continuation-node>

;; Detect memory leaks
(detect-memory-leaks snapshots)
;; => list of potential leaks
```

## Configuration

Default configuration options:

```scheme
'((max-trace-depth . 100)
  (memory-profiling . #t)
  (visualization . #t)
  (output-format . repl)
  (trace-continuations . #t)
  (profile-closures . #t)
  (break-on-error . #t)
  (continuation-chain-limit . 50)
  (memory-snapshot-interval . 1000)
  (verbose . #f)
  (color-output . #t))
```

## Development

### Running Tests

```bash
make check                    # Run all tests
./tests/run-tests.scm -s core # Run specific suite
./tests/run-tests.scm -b      # Run benchmarks
```

### Project Structure

```
guile-cps-debugger/
├── src/
│   ├── core/          # Core debugging engine
│   ├── analysis/      # Analysis modules
│   └── ui/            # User interface
├── tests/             # Test suite
├── examples/          # Example scripts
└── docs/              # Documentation
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the GNU Lesser General Public License v3.0 or later (LGPL-3.0+).

## Acknowledgments

This project builds upon Guile's excellent VM infrastructure and debugging facilities. Special thanks to the Guile development team and the Scheme community.

## Contact

For bug reports and feature requests, please use the issue tracker or contact bugs@example.org.