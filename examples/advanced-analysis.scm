#!/usr/bin/env guile3
!#

;;; advanced-analysis.scm --- Advanced analysis examples for CPS debugger
;;; Commentary:
;;;
;;; This example demonstrates advanced analysis features of the CPS debugger,
;;; including complex continuation patterns, memory leak detection, and
;;; performance analysis of different CPS transformations.
;;;
;;; Code:

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (cps-debugger core debugger)
             (cps-debugger analysis continuation)
             (cps-debugger analysis memory)
             (cps-debugger ui visualizer)
             (ice-9 format)
             (ice-9 pretty-print))

;;; Complex CPS examples

(define (tree-sum-cps tree k)
  "Sum all numbers in a tree structure using CPS."
  (cond
   ((null? tree) (k 0))
   ((number? tree) (k tree))
   ((pair? tree)
    (tree-sum-cps (car tree)
      (lambda (left)
        (tree-sum-cps (cdr tree)
          (lambda (right)
            (k (+ left right)))))))
   (else (k 0))))

(define (tree-map-cps f tree k)
  "Map function F over tree using CPS."
  (cond
   ((null? tree) (k '()))
   ((number? tree) (k (f tree)))
   ((pair? tree)
    (tree-map-cps f (car tree)
      (lambda (left)
        (tree-map-cps f (cdr tree)
          (lambda (right)
            (k (cons left right)))))))
   (else (k tree))))

(define (mutual-recursion-cps n)
  "Example of mutual recursion in CPS."
  (define (even? n k)
    (if (= n 0)
        (k #t)
        (odd? (- n 1) k)))
  
  (define (odd? n k)
    (if (= n 0)
        (k #f)
        (even? (- n 1) k)))
  
  (even? n (lambda (x) x)))

(define (accumulator-cps lst)
  "Example with multiple accumulators in CPS."
  (define (loop lst sum count max-val k)
    (if (null? lst)
        (k sum count max-val)
        (let ((val (car lst)))
          (loop (cdr lst)
                (+ sum val)
                (+ count 1)
                (max max-val val)
                k))))
  
  (loop lst 0 0 -inf.0 (lambda (s c m) (list s c m))))

;;; Analysis demonstrations

(define (analyze-tree-operations)
  "Analyze different tree operations."
  (format #t "\n=== Tree Operations Analysis ===\n")
  
  (let ((debugger (make-cps-debugger))
        (test-tree '(1 (2 3) (4 (5 6) (7 (8 9))))))
    
    ;; Analyze tree-sum
    (format #t "\nAnalyzing tree-sum-cps:\n")
    (format #t "Tree: ~a\n" test-tree)
    
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (tree-sum-cps test-tree (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data))
              (tree (build-continuation-tree trace-data)))
         
         (format #t "Result: ~a\n" result)
         (format #t "\nContinuation chain analysis:\n")
         (format #t "~a\n" (format-continuation-chain chain-info))
         
         ;; Check for cycles
         (let ((cycles (find-continuation-cycles 
                       (extract-continuation-chain trace-data))))
           (if (null? cycles)
               (format #t "No cycles detected.\n")
               (format #t "Warning: Cycles detected in: ~a\n" cycles))))))
    
    ;; Memory profile tree-map
    (format #t "\n\nMemory profiling tree-map-cps:\n")
    (call-with-values
     (lambda ()
       (profile-cps-memory
        (lambda ()
          (tree-map-cps (lambda (x) (* x x)) test-tree (lambda (x) x)))))
     (lambda (result report)
       (format #t "Result: ~a\n" result)
       (format #t "\n~a" (format-memory-report report))))))

(define (detect-memory-patterns)
  "Demonstrate memory leak detection."
  (format #t "\n=== Memory Pattern Detection ===\n")
  
  ;; Create a potentially leaky CPS function
  (define (leaky-cps n k)
    (let ((data (make-vector 1000 0)))  ; Allocate memory
      (if (= n 0)
          (k data)
          (leaky-cps (- n 1)
                     (lambda (result)
                       ;; Keep reference to data, creating a leak
                       (k (cons data result)))))))
  
  (format #t "\nAnalyzing potentially leaky function:\n")
  (call-with-values
   (lambda ()
     (profile-cps-memory
      (lambda () (leaky-cps 50 (lambda (x) x)))))
   (lambda (result report)
     (let ((leaks (report-leaks report)))
       (if (null? leaks)
           (format #t "No leaks detected.\n")
           (begin
             (format #t "Potential memory leaks detected:\n")
             (for-each
              (lambda (leak)
                (format #t "  - Severity: ~a, Growth rate: ~,2f%\n"
                        (assq-ref leak 'severity)
                        (* 100 (assq-ref leak 'growth-rate))))
              leaks)))))))

(define (compare-cps-strategies)
  "Compare different CPS transformation strategies."
  (format #t "\n=== CPS Strategy Comparison ===\n")
  
  (let ((debugger (make-cps-debugger))
        (n 100))
    
    ;; Strategy 1: Direct CPS
    (define (sum-direct-cps n k)
      (if (= n 0)
          (k 0)
          (sum-direct-cps (- n 1)
                          (lambda (r) (k (+ n r))))))
    
    ;; Strategy 2: Accumulator CPS
    (define (sum-acc-cps n acc k)
      (if (= n 0)
          (k acc)
          (sum-acc-cps (- n 1) (+ acc n) k)))
    
    ;; Strategy 3: Defunctionalized CPS
    (define (sum-defun-cps n k-rep)
      (define (apply-cont k-rep val)
        (match k-rep
          ('id val)
          (('add n next) (apply-cont next (+ n val)))))
      
      (if (= n 0)
          (apply-cont k-rep 0)
          (sum-defun-cps (- n 1) `(add ,n ,k-rep))))
    
    ;; Compare strategies
    (format #t "\nComparing CPS strategies for sum of 1..~a:\n\n" n)
    
    ;; Direct CPS
    (debugger-reset-state debugger)
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (sum-direct-cps n (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data))
              (stats (continuation-chain-statistics chain-info)))
         (format #t "Direct CPS:\n")
         (format #t "  Result: ~a\n" result)
         (format #t "  Max depth: ~a\n" (assq-ref stats 'max-depth))
         (format #t "  Continuations: ~a\n" 
                 (assq-ref stats 'total-continuations)))))
    
    ;; Accumulator CPS
    (debugger-reset-state debugger)
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (sum-acc-cps n 0 (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data))
              (stats (continuation-chain-statistics chain-info)))
         (format #t "\nAccumulator CPS:\n")
         (format #t "  Result: ~a\n" result)
         (format #t "  Max depth: ~a\n" (assq-ref stats 'max-depth))
         (format #t "  Continuations: ~a\n" 
                 (assq-ref stats 'total-continuations)))))
    
    ;; Defunctionalized CPS
    (format #t "\nDefunctionalized CPS:\n")
    (format #t "  Result: ~a\n" (sum-defun-cps n 'id))
    (format #t "  (Not traced - uses data structures instead of closures)\n")))

(define (visualize-complex-chain)
  "Demonstrate visualization of complex continuation chains."
  (format #t "\n=== Visualization Demo ===\n")
  
  (let ((debugger (make-cps-debugger)))
    
    ;; Create a complex continuation pattern
    (define (complex-cps n k)
      (define (helper1 n k)
        (if (< n 5)
            (helper2 n k)
            (helper1 (- n 1)
                     (lambda (r) (k (+ n r))))))
      
      (define (helper2 n k)
        (if (= n 0)
            (k 1)
            (helper1 (- n 1)
                     (lambda (r) (k (* n r))))))
      
      (helper1 n k))
    
    (format #t "\nTracing complex mutual recursion:\n")
    (call-with-values
     (lambda ()
       (debugger-trace-cps debugger
         (lambda () (complex-cps 8 (lambda (x) x)))))
     (lambda (result error state)
       (let* ((trace-data (state-trace-data state))
              (chain-info (analyze-continuation-chain trace-data)))
         
         (format #t "Result: ~a\n" result)
         
         ;; Generate ASCII visualization
         (format #t "\nASCII Visualization:\n")
         (display (ascii-tree-view chain-info))
         
         ;; Generate DOT graph
         (format #t "\n\nGraphviz DOT output:\n")
         (display (generate-dot-graph chain-info))
         (format #t "\n\n(Save the DOT output to a file and run: ")
         (format #t "dot -Tpng file.dot -o graph.png)\n"))))))

(define (performance-analysis)
  "Analyze performance characteristics."
  (format #t "\n=== Performance Analysis ===\n")
  
  (let ((sizes '(10 50 100 200)))
    
    (format #t "\nAnalyzing factorial-cps performance:\n")
    (format #t "Size | Time (ms) | Memory (KB) | GC runs\n")
    (format #t "--------------------------------------\n")
    
    (for-each
     (lambda (size)
       (let ((start-time (get-internal-real-time)))
         (call-with-values
          (lambda ()
            (profile-cps-memory
             (lambda ()
               (factorial-cps size (lambda (x) x)))))
          (lambda (result report)
            (let* ((end-time (get-internal-real-time))
                   (elapsed (* 1000.0 (/ (- end-time start-time)
                                        internal-time-units-per-second)))
                   (summary (report-summary report))
                   (memory (/ (assq-ref summary 'heap-growth) 1024))
                   (gc-runs (assq-ref summary 'gc-runs)))
              (format #t "~4d | ~9,2f | ~11,2f | ~7d\n"
                      size elapsed memory gc-runs))))))
     sizes)))

;;; Helper for creating large trees
(define (make-binary-tree depth)
  "Create a binary tree of given DEPTH."
  (if (= depth 0)
      1
      (cons (make-binary-tree (- depth 1))
            (make-binary-tree (- depth 1)))))

;;; Main program
(define (main)
  "Run all advanced analysis examples."
  (format #t "CPS Debugger Advanced Analysis Examples\n")
  (format #t "======================================\n")
  
  ;; Run demonstrations
  (analyze-tree-operations)
  (detect-memory-patterns)
  (compare-cps-strategies)
  (visualize-complex-chain)
  (performance-analysis)
  
  (format #t "\n\nAll advanced examples completed.\n"))

;; Run if executed directly
(when (eq? (current-filename) (car (command-line)))
  (main))