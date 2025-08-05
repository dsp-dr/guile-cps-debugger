#!/usr/bin/env guile
!#

;;; REPL integration demonstration

(use-modules (cps-debugger)
             (cps-debugger repl))

(display "=== CPS Debugger REPL Integration Demo ===\n\n")
(display "Available REPL commands:\n")
(display "  ,cps-debug FORM    - Debug CPS compilation of FORM\n")
(display "  ,cps-step FORM     - Step through CPS transformation\n")
(display "  ,cps-inspect FORM  - Inspect CPS structure\n")
(display "  ,cps-pretty FORM   - Pretty-print CPS\n\n")

(display "Example: Try these commands in the REPL:\n")
(display "  ,cps-pretty (lambda (x) (+ x 1))\n")
(display "  ,cps-inspect (lambda (x) (* x x))\n")
(display "  ,cps-debug (lambda (x) (if (> x 0) x 0))\n\n")

;; Start REPL
(display "Starting REPL with CPS debugger commands...\n")
(display "Type ,q to quit\n\n")

;; Use the built-in REPL
((@ (system repl repl) start-repl))