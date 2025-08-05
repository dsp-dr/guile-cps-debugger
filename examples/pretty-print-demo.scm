#!/usr/bin/env guile
!#

;;; Example: Pretty-printing CPS/pseudo-CPS

(add-to-load-path "..")
(use-modules (cps-debugger))

(display "=== Pretty-Printing Demo ===\n\n")

;; Example 1: Simple function
(display "1. Simple function pretty-printing:\n")
(display "   Original: (lambda (x) (+ x 1))\n\n")
(let ((cps (cps-step '(lambda (x) (+ x 1)))))
  (cps-pretty-print cps))

(display "\n" )
(display "="  65)
(display "\n\n")

;; Example 2: Nested expressions
(display "2. Nested expressions:\n")
(display "   Original: (lambda (x) (if (zero? x) 1 (* x (fact (- x 1)))))\n\n")
(let ((cps (cps-step '(lambda (x) 
                        (if (zero? x) 
                            1 
                            (* x (fact (- x 1))))))))
  (cps-pretty-print cps))

(display "\n")
(display "=" 65)
(display "\n\n")

;; Example 3: Let bindings
(display "3. Let bindings:\n")
(display "   Original: (let ((x 10) (y 20)) (+ x y))\n\n")
(let ((cps (cps-step '(let ((x 10) (y 20)) (+ x y)))))
  (cps-pretty-print cps))

(display "\n")
(display "=" 65)
(display "\n\n")

;; Example 4: Multiple expressions
(display "4. Sequential expressions:\n")
(display "   Original: (begin (display \"Hello\") (newline) 42)\n\n")
(let ((cps (cps-step '(begin 
                        (display "Hello")
                        (newline)
                        42))))
  (cps-pretty-print cps))

(display "\n\nThe pretty-printer formats the pseudo-CPS representation\n")
(display "with proper indentation and structure.\n")