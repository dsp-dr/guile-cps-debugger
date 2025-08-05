#!/usr/bin/env guile3
!#

;;; run-tests.scm --- Test runner for CPS debugger
;;; Commentary:
;;;
;;; This script runs all tests for the CPS debugger project.
;;; It can be invoked directly or through the build system.
;;;
;;; Code:

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (cps-debugger tests framework)
             (ice-9 getopt-long))

(define option-spec
  '((help (single-char #\h) (value #f))
    (verbose (single-char #\v) (value #f))
    (benchmark (single-char #\b) (value #f))
    (suite (single-char #\s) (value #t))))

(define (show-help)
  (display "Usage: run-tests.scm [OPTIONS]

Run tests for the CPS debugger.

Options:
  -h, --help        Show this help message
  -v, --verbose     Enable verbose output
  -b, --benchmark   Run performance benchmarks
  -s, --suite NAME  Run specific test suite

Available test suites:
  all               Run all tests (default)
  core              Core functionality tests
  memory            Memory profiling tests
  visualization     Visualization tests
  integration       Integration tests
"))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (verbose (option-ref options 'verbose #f))
         (benchmark (option-ref options 'benchmark #f))
         (suite (option-ref options 'suite "all")))
    
    (cond
     (help-wanted
      (show-help)
      (exit 0))
     
     (benchmark
      (benchmark-debugger)
      (exit 0))
     
     (else
      (when verbose
        (format #t "Running test suite: ~a\n" suite))
      
      (case (string->symbol suite)
        ((all) (run-all-tests))
        ((core) (run-suite "core-functionality"))
        ((memory) (run-suite "memory-profiling"))
        ((visualization) (run-suite "visualization"))
        ((integration) (run-suite "integration"))
        (else
         (format #t "Unknown test suite: ~a\n" suite)
         (exit 1)))
      
      (exit 0)))))

(define (run-suite name)
  "Run a specific test suite."
  (test-begin "cps-debugger")
  (test-group name
    (case (string->symbol name)
      ((core-functionality) (test-core-functionality))
      ((memory-profiling) (test-memory-profiling))
      ((visualization) (test-visualization))
      ((integration) (test-integration))))
  (test-end "cps-debugger"))

;; Make file executable
(main (command-line))