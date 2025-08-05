#!/usr/bin/env guile
!#

;;; Example: Inspecting lambda expressions

(add-to-load-path "..")
(use-modules (cps-debugger)
             (ice-9 pretty-print))

(display "=== Inspecting Lambda Expressions ===\n\n")

;; Example 1: Simple identity function
(display "1. Identity function: (lambda (x) x)\n")
(let* ((form '(lambda (x) x))
       (cps (cps-step form))
       (info (cps-inspect cps)))
  (display "\nInspection result:\n")
  (pretty-print info)
  (newline))

;; Example 2: Function with arithmetic
(display "2. Arithmetic function: (lambda (x y) (+ (* x x) (* y y)))\n")
(let* ((form '(lambda (x y) (+ (* x x) (* y y))))
       (cps (cps-step form))
       (info (cps-inspect cps)))
  (display "\nInspection result:\n")
  (pretty-print info)
  (newline))

;; Example 3: Conditional function
(display "3. Conditional function: (lambda (x) (if (> x 0) x (- x)))\n")
(let* ((form '(lambda (x) (if (> x 0) x (- x))))
       (cps (cps-step form))
       (info (cps-inspect cps)))
  (display "\nInspection result:\n")
  (pretty-print info)
  (newline))

;; Example 4: Let binding
(display "4. Let binding: (lambda (x) (let ((y (* x 2))) (+ x y)))\n")
(let* ((form '(lambda (x) (let ((y (* x 2))) (+ x y))))
       (cps (cps-step form))
       (info (cps-inspect cps)))
  (display "\nInspection result:\n")
  (pretty-print info)
  (newline))

(display "\nThe inspector shows the structure of each expression,\n")
(display "including types, arguments, and nested expressions.\n")