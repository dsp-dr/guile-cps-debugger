#!/usr/bin/env guile
!#

;;; Simple demonstration of CPS concepts in Guile

(use-modules (system base compile)
             (language tree-il)
             (ice-9 pretty-print))

(display "=== CPS Concepts Demo ===\n\n")

(display "This demo shows how Scheme code is compiled to Tree-IL\n")
(display "(CPS is an internal representation in Guile 3.0+)\n\n")

;; Example 1: Simple function
(display "1. Simple function: (lambda (x) (+ x 1))\n")
(display "   Tree-IL representation:\n")

(let ((tree-il (compile '(lambda (x) (+ x 1)) #:to 'tree-il)))
  (pretty-print tree-il)
  (newline))

;; Example 2: Multiple operations
(display "2. Function with multiple operations: (lambda (x y) (+ (* x x) (* y y)))\n")
(display "   Tree-IL representation:\n")

(let ((tree-il (compile '(lambda (x y) (+ (* x x) (* y y))) #:to 'tree-il)))
  (pretty-print tree-il)
  (newline))

;; Example 3: Conditional
(display "3. Conditional: (lambda (x) (if (> x 0) x (- x)))\n")
(display "   Tree-IL representation:\n")

(let ((tree-il (compile '(lambda (x) (if (> x 0) x (- x))) #:to 'tree-il)))
  (pretty-print tree-il)
  (newline))

;; Example 4: Recursive function
(display "4. Recursive factorial:\n")
(display "   (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n")
(display "   Tree-IL representation:\n")

(let ((tree-il (compile '(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) #:to 'tree-il)))
  (pretty-print tree-il)
  (newline))

(display "\nNote: Full CPS debugging requires Guile 3.0+\n")
(display "This demo shows Tree-IL, the intermediate form before CPS.\n")