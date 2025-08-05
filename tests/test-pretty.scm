#!/usr/bin/env guile
!#

;;; Test suite for cps-debugger pretty-printing functionality

(use-modules (cps-debugger pretty)
             (cps-debugger compat)
             (srfi srfi-64)
             (ice-9 receive))

(test-begin "cps-debugger-pretty")

(test-assert "cps-pretty-print handles const"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(const 42))))))
    (string-contains output "const")))

(test-assert "cps-pretty-print handles symbols"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print 'x)))))
    (string-contains output "x")))

(test-assert "cps-pretty-print handles call"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(call + x y))))))
    (string-contains output "call")))

(test-assert "cps-pretty-print handles primcall"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(primcall + x y))))))
    (string-contains output "primcall")))

(test-assert "cps-pretty-print handles if"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(if test then else))))))
    (string-contains output "if")))

(test-assert "cps-pretty-print handles lambda"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(lambda () body))))))
    (string-contains output "lambda")))

(test-assert "cps-pretty-print handles let"
  (let ((output (with-output-to-string
                  (lambda ()
                    (cps-pretty-print '(let ((x 1)) x))))))
    (string-contains output "let")))

(test-assert "pretty-print-expression works"
  (let ((output (with-output-to-string
                  (lambda ()
                    (pretty-print-expression '(const 42))))))
    (string-contains output "const")))

(test-assert "pretty-print-continuation works"
  (let ((output (with-output-to-string
                  (lambda ()
                    (pretty-print-continuation 'k '(lambda () body))))))
    (string-contains output "Continuation")))

(test-end "cps-debugger-pretty")