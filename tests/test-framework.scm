;;; test-framework.scm --- Testing framework for CPS debugger
;;; Commentary:
;;;
;;; This module provides testing utilities and framework for the CPS debugger,
;;; including test runners, benchmarking utilities, and test fixtures.
;;;
;;; Code:

(define-module (cps-debugger tests framework)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 time)
  #:use-module (cps-debugger core debugger)
  #:use-module (cps-debugger analysis continuation)
  #:use-module (cps-debugger analysis memory)
  #:export (run-all-tests
            test-cps-examples
            benchmark-debugger
            with-test-debugger
            define-cps-test
            assert-continuation-depth
            assert-memory-usage
            assert-no-leaks))

;;; Test runner configuration
(define (configure-test-runner)
  "Configure the test runner with custom settings."
  (let ((runner (test-runner-simple)))
    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (format #t "\n~a Testing ~a (~a tests)\n~a\n"
                (make-string 60 #\=)
                suite-name count
                (make-string 60 #\=))))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (case (test-result-kind runner)
          ((pass) (display "."))
          ((fail) (display "F"))
          ((xfail) (display "X"))
          ((skip) (display "S")))
        (force-output)))
    runner))

;;; Main test runner
(define (run-all-tests)
  "Run all CPS debugger tests."
  (test-runner-current (configure-test-runner))
  
  (test-begin "cps-debugger")
  
  ;; Core functionality tests
  (test-group "core-functionality"
    (test-core-functionality))
  
  ;; Memory profiling tests
  (test-group "memory-profiling"
    (test-memory-profiling))
  
  ;; Visualization tests
  (test-group "visualization"
    (test-visualization))
  
  ;; Integration tests
  (test-group "integration"
    (test-integration))
  
  (test-end "cps-debugger")
  
  ;; Display summary
  (let ((runner (test-runner-current)))
    (format #t "\n\nTest Summary:\n")
    (format #t "  Passed: ~a\n" (test-runner-pass-count runner))
    (format #t "  Failed: ~a\n" (test-runner-fail-count runner))
    (format #t "  Skipped: ~a\n" (test-runner-skip-count runner))))

;;; Test core functionality
(define (test-core-functionality)
  "Test core debugger functionality."
  
  (test-assert "debugger-creation"
    (cps-debugger? (make-cps-debugger)))
  
  (test-assert "option-management"
    (let ((dbg (make-cps-debugger)))
      (debugger-set-option! dbg 'test-option 42)
      (= (debugger-get-option dbg 'test-option) 42)))
  
  (test-assert "state-reset"
    (let ((dbg (make-cps-debugger)))
      (debugger-reset-state dbg)
      (debugger-state dbg)))
  
  (test-assert "hook-installation"
    (let ((dbg (make-cps-debugger)))
      (debugger-install-hooks dbg)
      (debugger-uninstall-hooks dbg)
      #t)))

;;; Test memory profiling
(define (test-memory-profiling)
  "Test memory profiling functionality."
  
  (test-assert "memory-report-creation"
    (let ((report (make-memory-report 
                   '((initial-heap . 1000))
                   '((final-heap . 2000))
                   '())))
      (memory-report? report)))
  
  (test-assert "memory-profiling-execution"
    (with-test-debugger
     (lambda (dbg)
       (call-with-values
        (lambda ()
          (profile-cps-memory 
           (lambda () 
             (let loop ((n 100))
               (if (zero? n)
                   'done
                   (cons n (loop (- n 1))))))))
        (lambda (result report)
          (and (eq? result 'done)
               (memory-report? report))))))))

;;; Test visualization
(define (test-visualization)
  "Test visualization functionality."
  
  (test-assert "ascii-tree-generation"
    (let ((chain-info (make-continuation-info 
                       5 
                       '((procedure . test) (arity . 2))
                       '((total-estimated . 1000))
                       '((length . 10)))))
      (string? (ascii-tree-view chain-info))))
  
  (test-assert "dot-graph-generation"
    (let ((chain-info (make-continuation-info
                       3
                       '((procedure . test))
                       '((total-estimated . 500))
                       '((length . 5)))))
      (and (string? (generate-dot-graph chain-info))
           (string-contains (generate-dot-graph chain-info) "digraph")))))

;;; Test integration
(define (test-integration)
  "Test integration of components."
  
  (test-assert "trace-and-analyze"
    (with-test-debugger
     (lambda (dbg)
       (call-with-values
        (lambda ()
          (debugger-trace-cps dbg
            (lambda ()
              (factorial-cps 5 (lambda (x) x)))))
        (lambda (result error state)
          (and (= result 120)
               (not error)
               state))))))
  
  (test-assert "profile-and-report"
    (with-test-debugger
     (lambda (dbg)
       (call-with-values
        (lambda ()
          (debugger-profile-memory dbg
            (lambda ()
              (fibonacci-cps 10 (lambda (x) x)))))
        (lambda (result error report)
          (and (number? result)
               (not error)
               (memory-report? report))))))))

;;; Test fixtures and helpers

(define (with-test-debugger proc)
  "Execute PROC with a test debugger instance."
  (let ((dbg (make-cps-debugger)))
    (proc dbg)))

(define-syntax define-cps-test
  (syntax-rules ()
    ((define-cps-test name expr expected)
     (test-equal name expected
       (with-test-debugger
        (lambda (dbg)
          (call-with-values
           (lambda () (debugger-trace-cps dbg (lambda () expr)))
           (lambda (result error state)
             (if error
                 (error "Test failed" error)
                 result)))))))))

;;; Assertion helpers

(define (assert-continuation-depth dbg thunk max-depth)
  "Assert that continuation depth doesn't exceed MAX-DEPTH."
  (call-with-values
   (lambda () (debugger-trace-cps dbg thunk))
   (lambda (result error state)
     (if error
         #f
         (let* ((trace-data (state-trace-data state))
                (chain-info (analyze-continuation-chain trace-data))
                (depth (continuation-depth chain-info)))
           (<= depth max-depth))))))

(define (assert-memory-usage dbg thunk max-bytes)
  "Assert that memory usage doesn't exceed MAX-BYTES."
  (call-with-values
   (lambda () (profile-cps-memory thunk))
   (lambda (result report)
     (let* ((summary (report-summary report))
            (growth (assq-ref summary 'heap-growth)))
       (<= growth max-bytes)))))

(define (assert-no-leaks dbg thunk)
  "Assert that no memory leaks are detected."
  (call-with-values
   (lambda () (profile-cps-memory thunk))
   (lambda (result report)
     (null? (report-leaks report)))))

;;; Benchmark utilities

(define (benchmark-debugger)
  "Run performance benchmarks for the debugger."
  (format #t "\nRunning CPS Debugger Benchmarks\n")
  (format #t "================================\n\n")
  
  ;; Benchmark trace overhead
  (benchmark-trace-overhead)
  
  ;; Benchmark memory profiling
  (benchmark-memory-profiling)
  
  ;; Benchmark analysis
  (benchmark-analysis))

(define (benchmark-trace-overhead)
  "Benchmark tracing overhead."
  (format #t "Trace Overhead Benchmark:\n")
  
  (let ((dbg (make-cps-debugger))
        (iterations 1000))
    
    ;; Baseline without debugging
    (let ((baseline-time
           (time-execution
            (lambda ()
              (let loop ((n iterations))
                (if (zero? n)
                    'done
                    (begin
                      (factorial-cps 10 (lambda (x) x))
                      (loop (- n 1)))))))))
      
      ;; With debugging enabled
      (let ((debug-time
             (time-execution
              (lambda ()
                (let loop ((n iterations))
                  (if (zero? n)
                      'done
                      (begin
                        (debugger-trace-cps dbg
                          (lambda ()
                            (factorial-cps 10 (lambda (x) x))))
                        (loop (- n 1)))))))))
        
        (format #t "  Baseline: ~,3f seconds\n" baseline-time)
        (format #t "  With debugging: ~,3f seconds\n" debug-time)
        (format #t "  Overhead: ~,1f%\n\n" 
                (* 100 (/ (- debug-time baseline-time) baseline-time)))))))

(define (benchmark-memory-profiling)
  "Benchmark memory profiling performance."
  (format #t "Memory Profiling Benchmark:\n")
  
  (let ((iterations 100))
    (let ((profile-time
           (time-execution
            (lambda ()
              (let loop ((n iterations))
                (if (zero? n)
                    'done
                    (begin
                      (profile-cps-memory
                       (lambda ()
                         (tree-sum-cps '(1 (2 3) (4 (5 6))) 
                                       (lambda (x) x))))
                      (loop (- n 1)))))))))
      
      (format #t "  ~a iterations: ~,3f seconds\n" iterations profile-time)
      (format #t "  Average per iteration: ~,3f ms\n\n"
              (* 1000 (/ profile-time iterations))))))

(define (benchmark-analysis)
  "Benchmark analysis performance."
  (format #t "Analysis Performance Benchmark:\n")
  
  ;; Create large trace data
  (let* ((trace-size 10000)
         (trace-data (generate-test-trace trace-size))
         (analysis-time
          (time-execution
           (lambda ()
             (analyze-continuation-chain trace-data)))))
    
    (format #t "  Analyzing ~a entries: ~,3f seconds\n" 
            trace-size analysis-time)
    (format #t "  Average per entry: ~,3f µs\n\n"
            (* 1000000 (/ analysis-time trace-size)))))

;;; Test data generators

(define (generate-test-trace size)
  "Generate test trace data of given SIZE."
  (map (lambda (i)
         `((procedure . ,(string->symbol (format #f "proc~a" i)))
           (arguments . (,i ,(lambda (x) x)))
           (time . ,i)))
       (iota size)))

;;; Time execution helper
(define (time-execution thunk)
  "Return execution time of THUNK in seconds."
  (let ((start (get-internal-real-time)))
    (thunk)
    (/ (- (get-internal-real-time) start)
       internal-time-units-per-second)))

;;; Example CPS functions for testing

(define (factorial-cps n k)
  "Factorial in CPS for testing."
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (r) (k (* n r))))))

(define (fibonacci-cps n k)
  "Fibonacci in CPS for testing."
  (cond
   ((= n 0) (k 0))
   ((= n 1) (k 1))
   (else
    (fibonacci-cps (- n 1)
      (lambda (a)
        (fibonacci-cps (- n 2)
          (lambda (b)
            (k (+ a b)))))))))

(define (tree-sum-cps tree k)
  "Tree sum in CPS for testing."
  (cond
   ((null? tree) (k 0))
   ((number? tree) (k tree))
   (else
    (tree-sum-cps (car tree)
      (lambda (left)
        (tree-sum-cps (cdr tree)
          (lambda (right)
            (k (+ left right)))))))))