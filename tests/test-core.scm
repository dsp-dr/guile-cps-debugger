#!/usr/bin/env guile
!#

;;; Test suite for cps-debugger core functionality

(use-modules (cps-debugger core)
             (srfi srfi-64))

(test-begin "cps-debugger-core")

(test-assert "make-cps-debugger creates debugger"
  (cps-debugger? (make-cps-debugger #f)))

(test-assert "cps-debug returns debugger"
  (cps-debugger? (cps-debug '(lambda (x) x))))

(test-assert "cps-step returns CPS"
  (cps-step '(lambda (x) (+ x 1))))

(test-assert "debugger-step enables stepping"
  (let ((dbg (make-cps-debugger #f)))
    (debugger-step dbg)
    (debugger-stepping? dbg)))

(test-assert "debugger-continue disables stepping"
  (let ((dbg (make-cps-debugger #f)))
    (debugger-step dbg)
    (debugger-continue dbg)
    (not (debugger-stepping? dbg))))

(test-equal "debugger-break adds breakpoint"
  '(test-location)
  (let ((dbg (make-cps-debugger #f)))
    (debugger-break dbg 'test-location)
    (debugger-breakpoints dbg)))

(test-end "cps-debugger-core")