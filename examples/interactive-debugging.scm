#!/usr/bin/env guile
!#

;;; Example: Interactive debugging session

(add-to-load-path "..")
(use-modules (cps-debugger)
             (cps-debugger stepper)
             (cps-debugger session)
             (cps-debugger cli))

(display "=== Interactive Debugging Example ===\n\n")

;; Example 1: Simple stepping through a function
(display "Example 1: Step-through debugging\n")
(display "Function: (lambda (x) (if (> x 0) (* x 2) (- x)))\n\n")

(let* ((form '(lambda (x) (if (> x 0) (* x 2) (- x))))
       (cps (cps-step form))
       (stepper (make-stepper cps)))
  
  (display "Initial state:\n")
  (stepper-print-state stepper)
  
  (display "\nStepping forward:\n")
  (step-forward stepper)
  (stepper-print-state stepper)
  
  (display "\nStepping into:\n")
  (step-into stepper)
  (stepper-print-state stepper)
  
  (display "\nStepping back:\n")
  (step-backward stepper)
  (stepper-print-state stepper))

(display "\n")
(display "=" 65)
(display "\n\n")

;; Example 2: Using breakpoints
(display "Example 2: Breakpoint debugging\n")
(display "Function: (lambda (x y) (let ((a (+ x y)) (b (* x y))) (- a b)))\n\n")

(let* ((form '(lambda (x y) 
                (let ((a (+ x y))
                      (b (* x y)))
                  (- a b))))
       (cps (cps-step form))
       (stepper (make-stepper cps)))
  
  ;; Add breakpoints
  (add-breakpoint stepper '(0))
  (add-breakpoint stepper '(0 1))
  
  (display "Breakpoints set at positions (0) and (0 1)\n")
  (display "Current breakpoints:\n")
  (for-each (lambda (bp)
              (format #t "  ~s\n" bp))
            (list-breakpoints stepper))
  
  (display "\nContinuing to first breakpoint:\n")
  (step-to-breakpoint stepper)
  (format #t "Stopped at position: ~s\n" (stepper-position stepper))
  (format #t "State: ~a\n" (stepper-state stepper)))

(display "\n")
(display "=" 65)
(display "\n\n")

;; Example 3: Debug session with watches
(display "Example 3: Debug session with watches\n\n")

(let* ((form '(lambda (n)
                (let loop ((i n) (sum 0))
                  (if (zero? i)
                      sum
                      (loop (- i 1) (+ sum i))))))
       (cps (cps-step form))
       (session (make-debug-session cps)))
  
  ;; Add watch expressions
  (add-watch session 'position 
             (lambda (ctx) 
               (stepper-position (session-stepper session))))
  (add-watch session 'node-count
             (lambda (ctx)
               (count-nodes ctx)))
  
  (display "Session created with watches:\n")
  (for-each (lambda (watch)
              (format #t "  ~a: ~s\n" (car watch) (cdr watch)))
            (list-watches session))
  
  (display "\nEvaluating watches:\n")
  (for-each (lambda (result)
              (format #t "  ~a = ~s\n" (car result) (cdr result)))
            (evaluate-watches session))
  
  (display "\nSession summary:\n")
  (session-summary session))

(display "\n")
(display "=" 65)
(display "\n\n")

;; Example 4: Interactive CLI demo
(display "Example 4: Starting interactive CLI\n")
(display "To start an interactive debugging session, run:\n")
(display "  (start-debugger-cli <your-cps-term>)\n")
(display "\nAvailable commands in CLI:\n")
(display "  step, back, into, over, out - Navigation\n")
(display "  break, unbreak, continue    - Breakpoints\n")
(display "  inspect, pretty, analyze    - Inspection\n")
(display "  watch, unwatch              - Watch expressions\n")
(display "  save, load                  - Session persistence\n")
(display "  help, quit                  - Help and exit\n")

(display "\nTry it with:\n")
(display "  (start-debugger-cli (cps-step '(lambda (x) (+ x 1))))\n")