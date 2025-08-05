#!/usr/bin/env guile
!#

;;; Test suite for cps-debugger compatibility layer

(use-modules (cps-debugger compat)
             (system base compile)
             (language tree-il)
             (srfi srfi-64))

(test-begin "cps-debugger-compat")

(test-assert "cps-available? returns boolean"
  (boolean? (cps-available?)))

(test-assert "compile-to-cps-compat works"
  (compile-to-cps-compat '(lambda (x) (+ x 1))))

(test-assert "tree-il->pseudo-cps handles lambda"
  (let* ((tree-il (compile '(lambda (x) x) #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'lambda))))

(test-assert "tree-il->pseudo-cps handles const"
  (let* ((tree-il (compile 42 #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'const)
         (= (cadr pseudo) 42))))

(test-assert "tree-il->pseudo-cps handles primcall"
  (let* ((tree-il (compile '(+ 1 2) #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'primcall))))

(test-assert "tree-il->pseudo-cps handles conditional"
  (let* ((tree-il (compile '(if #t 1 2) #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'if))))

(test-assert "tree-il->pseudo-cps handles let"
  (let* ((tree-il (compile '(let ((x 1)) x) #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'let))))

(test-assert "tree-il->pseudo-cps handles begin"
  (let* ((tree-il (compile '(begin 1 2) #:to 'tree-il))
         (pseudo (tree-il->pseudo-cps tree-il)))
    (and (pair? pseudo)
         (eq? (car pseudo) 'begin))))

(test-end "cps-debugger-compat")