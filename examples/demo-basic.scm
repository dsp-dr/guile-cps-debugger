#!/usr/bin/env guile
!#

;;; Basic demonstration of the CPS debugger

(use-modules (cps-debugger))

(display "=== CPS Debugger Basic Demo ===\n\n")

;; Example 1: Simple function
(display "1. Debugging a simple function:\n")
(display "   (lambda (x) (+ x 1))\n\n")

(let ((result (cps-step '(lambda (x) (+ x 1)))))
  (display "CPS representation:\n")
  (cps-pretty-print result))

(newline)
(display "Press Enter to continue...")
(read-line)

;; Example 2: Function with multiple operations
(display "\n2. Function with multiple operations:\n")
(display "   (lambda (x y) (+ (* x x) (* y y)))\n\n")

(let ((result (cps-step '(lambda (x y) (+ (* x x) (* y y))))))
  (display "CPS representation:\n")
  (cps-pretty-print result))

(newline)
(display "Press Enter to continue...")
(read-line)

;; Example 3: Conditional expression
(display "\n3. Conditional expression:\n")
(display "   (lambda (x) (if (> x 0) x (- x)))\n\n")

(let ((result (cps-step '(lambda (x) (if (> x 0) x (- x))))))
  (display "CPS representation:\n")
  (cps-pretty-print result))

(newline)
(display "Press Enter to continue...")
(read-line)

;; Example 4: Using the inspector
(display "\n4. Inspecting CPS structure:\n")
(let* ((cps (cps-step '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))
       (info (cps-inspect cps)))
  (display "Inspection results:\n")
  (for-each (lambda (pair)
              (format #t "  ~a: ~s\n" (car pair) (cdr pair)))
            info))

(newline)
(display "Demo completed!\n")