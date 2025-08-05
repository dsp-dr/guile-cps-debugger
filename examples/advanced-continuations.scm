;;; advanced-continuations.scm --- Advanced continuation patterns and examples
;;;
;;; This file demonstrates the flexibility of continuations in CPS,
;;; showing how any function can serve as a continuation.

(use-modules (ice-9 format)
             (srfi srfi-1))

;;; Basic CPS functions for demonstration

(define (fibonacci-cps n k)
  "Fibonacci in continuation-passing style"
  (if (< n 2)
      (k n)
      (fibonacci-cps (- n 1)
                     (lambda (fib-n-1)
                       (fibonacci-cps (- n 2)
                                      (lambda (fib-n-2)
                                        (k (+ fib-n-1 fib-n-2))))))))

(define (factorial-cps n k)
  "Factorial in continuation-passing style"
  (if (= n 0)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (result)
                       (k (* n result))))))

;;; Section 1: Simple Transformation Continuations

(display "=== Simple Transformation Continuations ===\n\n")

;; Identity - the simplest continuation
(define (identity x) x)
(format #t "fib(5) with identity: ~a\n" 
        (fibonacci-cps 5 identity))

;; Add a constant
(define (add-10 x) (+ x 10))
(format #t "fib(5) + 10: ~a\n" 
        (fibonacci-cps 5 add-10))

;; Create parameterized continuations
(define (make-adder n)
  (lambda (x) (+ x n)))

(format #t "fib(6) + 100: ~a\n" 
        (fibonacci-cps 6 (make-adder 100)))

;; Multiply by constant
(define (double x) (* 2 x))
(format #t "fib(7) * 2: ~a\n" 
        (fibonacci-cps 7 double))

;;; Section 2: Accumulator Continuations

(display "\n=== Accumulator Continuations ===\n\n")

;; Stateful accumulator (not pure functional)
(define sum-accumulator 0)
(define (sum-cont x)
  (set! sum-accumulator (+ sum-accumulator x))
  (format #t "  Added ~a, total now: ~a\n" x sum-accumulator)
  sum-accumulator)

(display "Accumulating fibonacci values:\n")
(fibonacci-cps 3 sum-cont)
(fibonacci-cps 4 sum-cont)
(fibonacci-cps 5 sum-cont)
(format #t "Final sum: ~a\n" sum-accumulator)

;; Reset for next example
(set! sum-accumulator 0)

;;; Section 3: Collection Continuations

(display "\n=== Collection Continuations ===\n\n")

(define results '())
(define (collect-cont x)
  (set! results (cons x results))
  (format #t "  Collected: ~a\n" x)
  x)

(display "Collecting fibonacci values:\n")
(fibonacci-cps 4 collect-cont)
(fibonacci-cps 5 collect-cont)
(fibonacci-cps 6 collect-cont)
(format #t "All results: ~a\n" (reverse results))

;;; Section 4: Continuation Composition

(display "\n=== Continuation Composition ===\n\n")

(define (compose-cont f g)
  "Compose two continuations: (f ∘ g)(x) = f(g(x))"
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (add-5 x) (+ x 5))

;; Pipeline: fib -> double -> add-5 -> square
(define pipeline 
  (compose-cont square 
                (compose-cont add-5 double)))

(format #t "fib(5) -> *2 -> +5 -> square: ~a\n"
        (fibonacci-cps 5 pipeline))
(format #t "  Breakdown: 5 -> 10 -> 15 -> 225\n")

;;; Section 5: Conditional Continuations

(display "\n=== Conditional Continuations ===\n\n")

(define (classify-even-odd x)
  (if (even? x)
      (cons 'even x)
      (cons 'odd x)))

(format #t "fib(6) classified: ~a\n" 
        (fibonacci-cps 6 classify-even-odd))
(format #t "fib(7) classified: ~a\n" 
        (fibonacci-cps 7 classify-even-odd))

(define (threshold-check limit)
  (lambda (x)
    (if (> x limit)
        (format #f "HIGH: ~a (exceeds ~a)" x limit)
        (format #f "LOW: ~a (under ~a)" x limit))))

(format #t "fib(10) with threshold 50: ~a\n"
        (fibonacci-cps 10 (threshold-check 50)))
(format #t "fib(10) with threshold 100: ~a\n"
        (fibonacci-cps 10 (threshold-check 100)))

;;; Section 6: Multi-Value Continuations

(display "\n=== Multi-Value Continuations ===\n\n")

(define (stats-cont x)
  "Return multiple statistics about the value"
  (list (cons 'value x)
        (cons 'double (* 2 x))
        (cons 'square (* x x))
        (cons 'even? (even? x))))

(format #t "fib(8) statistics: ~a\n"
        (fibonacci-cps 8 stats-cont))

;;; Section 7: Sum of First N Fibonacci Numbers

(display "\n=== Practical Example: Sum of First N Fibs ===\n\n")

(define (sum-first-n-fibs n)
  "Pure functional version using CPS"
  (define (loop i acc k)
    (if (>= i n)
        (k acc)
        (fibonacci-cps i
                       (lambda (fib-i)
                         (loop (+ i 1) 
                               (+ acc fib-i)
                               k)))))
  (loop 0 0 identity))

(format #t "Sum of first 8 fibonacci numbers: ~a\n"
        (sum-first-n-fibs 8))
(format #t "  (Should be: 0+1+1+2+3+5+8+13 = 33)\n")

;;; Section 8: Combining Multiple CPS Computations

(display "\n=== Combining Multiple CPS Computations ===\n\n")

(define (sum-two-fibs n1 n2)
  "Add two fibonacci numbers using nested CPS"
  (fibonacci-cps n1
                 (lambda (fib-n1)
                   (fibonacci-cps n2
                                  (lambda (fib-n2)
                                    (+ fib-n1 fib-n2))))))

(format #t "fib(5) + fib(7) = ~a\n"
        (sum-two-fibs 5 7))
(format #t "  (Should be: 5 + 13 = 18)\n")

(define (product-three-fibs n1 n2 n3)
  "Multiply three fibonacci numbers"
  (fibonacci-cps n1
                 (lambda (f1)
                   (fibonacci-cps n2
                                  (lambda (f2)
                                    (fibonacci-cps n3
                                                   (lambda (f3)
                                                     (* f1 f2 f3))))))))

(format #t "fib(4) * fib(5) * fib(6) = ~a\n"
        (product-three-fibs 4 5 6))
(format #t "  (Should be: 3 * 5 * 8 = 120)\n")

;;; Section 9: Tracing Continuations

(display "\n=== Tracing Continuations ===\n\n")

(define (make-tracer name)
  "Create a tracing continuation"
  (lambda (x)
    (format #t "[~a] Received: ~a\n" name x)
    x))

(define (traced-pipeline x)
  ((compose-cont (make-tracer "final")
                 (compose-cont square
                              (compose-cont (make-tracer "after-add")
                                          (compose-cont add-5
                                                       (make-tracer "after-double")))))
   (* 2 x)))

(display "Traced pipeline for fib(4):\n")
(fibonacci-cps 4 
               (lambda (x) 
                 (format #t "[initial] fib(4) = ~a\n" x)
                 (traced-pipeline x)))

;;; Section 10: Error Handling Continuations

(display "\n=== Error Handling Continuations ===\n\n")

(define (safe-divide-cont divisor)
  (lambda (x)
    (if (= divisor 0)
        (format #f "ERROR: Division by zero (value was ~a)" x)
        (/ x divisor))))

(format #t "fib(10) / 5 = ~a\n"
        (fibonacci-cps 10 (safe-divide-cont 5)))
(format #t "fib(10) / 0 = ~a\n"
        (fibonacci-cps 10 (safe-divide-cont 0)))

;;; Section 11: Continuation with Side Effects

(display "\n=== Continuations with Side Effects ===\n\n")

(define output-log '())

(define (logging-cont prefix)
  (lambda (x)
    (let ((msg (format #f "~a: ~a" prefix x)))
      (set! output-log (cons msg output-log))
      (display msg)
      (newline)
      x)))

(fibonacci-cps 5 (logging-cont "Result"))
(factorial-cps 5 (logging-cont "Factorial"))

(display "\nLog history:\n")
(for-each (lambda (msg) 
            (format #t "  - ~a\n" msg))
          (reverse output-log))

;;; Section 12: Memoizing Continuation

(display "\n=== Memoizing Continuation ===\n\n")

(define memo-table (make-hash-table))

(define (memoizing-cont key)
  (lambda (value)
    (hash-set! memo-table key value)
    (format #t "Memoized ~a => ~a\n" key value)
    value))

(fibonacci-cps 10 (memoizing-cont 'fib-10))
(factorial-cps 7 (memoizing-cont 'fact-7))

(display "\nMemo table contents:\n")
(hash-for-each (lambda (k v)
                 (format #t "  ~a: ~a\n" k v))
               memo-table)

;;; Summary

(display "\n=== Summary ===\n\n")
(display "Continuations in CPS are incredibly flexible:\n")
(display "1. They can transform values (arithmetic, formatting)\n")
(display "2. They can accumulate or collect results\n")
(display "3. They can be composed like functions\n")
(display "4. They can make decisions (conditional logic)\n")
(display "5. They can handle errors gracefully\n")
(display "6. They can produce side effects (logging, memoization)\n")
(display "7. They can combine multiple CPS computations\n")
(display "\nThe continuation is simply a function that receives\n")
(display "the result and decides what to do with it!\n")