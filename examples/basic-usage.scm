#!/usr/bin/env guile3
!#

;;; basic-usage.scm --- Basic usage examples for CPS debugger
;;; Commentary:
;;;
;;; This example demonstrates basic usage of the CPS debugger,
;;; including tracing simple CPS functions and analyzing their
;;; continuation chains.
;;;
;;; Code:

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (cps-debugger core debugger)
             (cps-debugger analysis continuation)
             (cps-debugger analysis memory)
             (ice-9 format))

;;; Example 1: Simple factorial in CPS
(define (factorial-cps n k)
  "Calculate factorial of N in continuation-passing style."
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (r) (k (* n r))))))

;;; Example 2: Fibonacci in CPS
(define (fibonacci-cps n k)
  "Calculate Nth Fibonacci number in continuation-passing style."
  (cond
   ((= n 0) (k 0))
   ((= n 1) (k 1))
   (else
    (fibonacci-cps (- n 1)
      (lambda (a)
        (fibonacci-cps (- n 2)
          (lambda (b)
            (k (+ a b)))))))))

;;; Example 3: List sum in CPS
(define (list-sum-cps lst k)
  "Sum elements of LST in continuation-passing style."
  (if (null? lst)
      (k 0)
      (list-sum-cps (cdr lst)
                    (lambda (rest-sum)
                      (k (+ (car lst) rest-sum))))))

;;; Demonstration functions

(define (demo-factorial)
  "Demonstrate factorial CPS debugging."
  (format #t "\n=== Factorial CPS Demo ===\n")
  
  ;; Create debugger
  (let ((debugger (make-cps-debugger)))
    
    ;; Trace factorial computation
    (format #t "\nTracing factorial-cps 5:\n")
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger 
         (lambda () (factorial-cps 5 (lambda (x) x)))))
     (lambda (result error state)
       (if error
           (format #t "Error: ~a\n" error)
           (begin
             (format #t "Result: ~a\n" result)
             
             ;; Analyze continuation chain
             (let* ((trace-data (state-trace-data state))
                    (chain-info (analyze-continuation-chain trace-data)))
               (format #t "\n~a\n" (format-continuation-chain chain-info)))))))))

(define (demo-fibonacci)
  "Demonstrate Fibonacci CPS debugging with memory profiling."
  (format #t "\n=== Fibonacci CPS Demo ===\n")
  
  ;; Profile memory usage
  (format #t "\nProfiling fibonacci-cps 10:\n")
  (call-with-values
   (lambda ()
     (profile-cps-memory 
      (lambda () (fibonacci-cps 10 (lambda (x) x)))))
   (lambda (result report)
     (format #t "Result: ~a\n" result)
     (format #t "\n~a" (format-memory-report report)))))

(define (demo-list-sum)
  "Demonstrate list sum CPS with custom analysis."
  (format #t "\n=== List Sum CPS Demo ===\n")
  
  (let ((debugger (make-cps-debugger))
        (test-list '(1 2 3 4 5 6 7 8 9 10)))
    
    ;; Configure debugger
    (debugger-set-option! debugger 'max-trace-depth 50)
    (debugger-set-option! debugger 'verbose #t)
    
    (format #t "\nTracing sum of ~a:\n" test-list)
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (list-sum-cps test-list (lambda (x) x)))))
     (lambda (result error state)
       (if error
           (format #t "Error: ~a\n" error)
           (begin
             (format #t "Result: ~a\n" result)
             
             ;; Show statistics
             (let* ((trace-data (state-trace-data state))
                    (chain-info (analyze-continuation-chain trace-data))
                    (stats (continuation-chain-statistics chain-info)))
               (format #t "\nStatistics:\n")
               (format #t "  Max depth: ~a\n" (assq-ref stats 'max-depth))
               (format #t "  Total continuations: ~a\n" 
                       (assq-ref stats 'total-continuations))
               (format #t "  Memory per continuation: ~,2f bytes\n"
                       (assq-ref stats 'memory-per-continuation)))))))))

(define (demo-comparison)
  "Compare different CPS patterns."
  (format #t "\n=== CPS Pattern Comparison ===\n")
  
  (let ((debugger (make-cps-debugger)))
    
    ;; Compare factorial implementations
    (format #t "\nComparing factorial implementations:\n")
    
    ;; Standard CPS
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (factorial-cps 8 (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data)))
         (format #t "Standard CPS - Depth: ~a, Closures: ~a\n"
                 (continuation-depth chain-info)
                 (length (continuation-closures chain-info))))))
    
    ;; Tail-recursive version (for comparison)
    (define (factorial-iter n acc k)
      (if (= n 0)
          (k acc)
          (factorial-iter (- n 1) (* n acc) k)))
    
    (debugger-reset-state debugger)
    
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (factorial-iter 8 1 (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data)))
         (format #t "Iterative CPS - Depth: ~a, Closures: ~a\n"
                 (continuation-depth chain-info)
                 (length (continuation-closures chain-info))))))))

;;; Main program
(define (main)
  "Run all demonstrations."
  (format #t "CPS Debugger Basic Usage Examples\n")
  (format #t "=================================\n")
  
  ;; Run demos
  (demo-factorial)
  (demo-fibonacci)
  (demo-list-sum)
  (demo-comparison)
  
  (format #t "\n\nAll demos completed.\n"))

;; Run if executed directly
(when (eq? (current-filename) (car (command-line)))
  (main))