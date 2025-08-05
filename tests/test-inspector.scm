#!/usr/bin/env guile
!#

;;; Test suite for cps-debugger inspector functionality

(use-modules (cps-debugger inspector)
             (cps-debugger compat)
             (srfi srfi-64))

(test-begin "cps-debugger-inspector")

(test-assert "cps-inspect handles symbols"
  (let ((result (cps-inspect 'x)))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'variable)
         (eq? (assq-ref result 'name) 'x))))

(test-assert "cps-inspect handles const"
  (let ((result (cps-inspect '(const 42))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'const)
         (= (assq-ref result 'value) 42))))

(test-assert "cps-inspect handles call"
  (let ((result (cps-inspect '(call + x y))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'call))))

(test-assert "cps-inspect handles primcall"
  (let ((result (cps-inspect '(primcall + x y))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'primcall)
         (eq? (assq-ref result 'name) '+))))

(test-assert "cps-inspect handles if"
  (let ((result (cps-inspect '(if test then else))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'conditional))))

(test-assert "cps-inspect handles lambda"
  (let ((result (cps-inspect '(lambda () body))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'lambda))))

(test-assert "inspect-term works"
  (let ((result (inspect-term '(const 123))))
    (and (assq 'type result)
         (eq? (assq-ref result 'type) 'const))))

(test-assert "list-continuations returns empty for pseudo-CPS"
  (null? (list-continuations '(lambda () (const 42)))))

(test-assert "find-continuation returns #f for pseudo-CPS"
  (not (find-continuation 'k '(lambda () (const 42)))))

(test-end "cps-debugger-inspector")