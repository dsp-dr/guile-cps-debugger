#!/usr/bin/env guile3
!#

;;; Test script for the visualizer module

(use-modules (cps-debugger)
             (cps-debugger visualizer)
             (system vm debug)
             (ice-9 format))

(display "=== Testing CPS Debugger Visualizer ===\n\n")

;; Test 1: Stack visualization
(display "Test 1: Stack Visualization\n")
(display "----------------------------\n")
(define (recursive-test n)
  (if (= n 0)
      (begin
        (display (stack->mermaid (make-stack #t)))
        'done)
      (recursive-test (- n 1))))

(recursive-test 3)
(newline)

;; Test 2: Evaluation model diagram
(display "\nTest 2: Evaluation Model Diagram\n")
(display "---------------------------------\n")
(display (eval-model->diagram '(factorial 5)))
(newline)

;; Test 3: CPS factorial with visualization
(display "\nTest 3: CPS Factorial Visualization\n")
(display "------------------------------------\n")
(define (factorial-cps n k)
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (result)
                       (k (* n result))))))

(define (identity x) x)

;; Create a traced version
(define (factorial-cps-traced n k depth)
  (format #t "~vTfactorial-cps(~a)\n" (* depth 2) #\space n)
  (if (= n 0)
      (begin
        (format #t "~vT└─> base case: 1\n" (* depth 2) #\space)
        (k 1))
      (factorial-cps-traced 
        (- n 1)
        (lambda (result)
          (format #t "~vT└─> continuation: ~a * ~a = ~a\n" 
                  (* depth 2) #\space n result (* n result))
          (k (* n result)))
        (+ depth 1))))

(display "Traced execution:\n")
(factorial-cps-traced 4 identity 0)
(newline)

;; Test 4: Simple Mermaid diagram
(display "\nTest 4: Simple Mermaid Stack Diagram\n")
(display "-------------------------------------\n")
(display "graph TD\n")
(display "    A[Start] --> B[factorial-cps 3]\n")
(display "    B --> C[factorial-cps 2]\n")
(display "    C --> D[factorial-cps 1]\n")
(display "    D --> E[factorial-cps 0]\n")
(display "    E --> F[base case: 1]\n")
(display "    F --> G[apply continuations]\n")
(display "    G --> H[Result: 6]\n")
(newline)

(display "\n=== All tests completed ===\n")