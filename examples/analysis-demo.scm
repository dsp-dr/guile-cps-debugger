#!/usr/bin/env guile
!#

;;; Example: Static analysis of code

(add-to-load-path "..")
(use-modules (cps-debugger)
             (cps-debugger analysis)
             (ice-9 pretty-print))

(display "=== Static Analysis Demo ===\n\n")

;; Example 1: Analyze simple function
(display "1. Analyzing simple function: (lambda (x y) (+ x y))\n")
(let* ((form '(lambda (x y) (+ x y)))
       (cps (cps-step form))
       (analysis (analyze-cps cps)))
  (display "\nAnalysis results:\n")
  (for-each (lambda (pair)
              (format #t "  ~a: ~s\n" (car pair) (cdr pair)))
            analysis)
  (newline))

;; Example 2: Analyze function with calls
(display "2. Analyzing function with multiple calls:\n")
(display "   (lambda (x) (+ (* x x) (sqrt x)))\n")
(let* ((form '(lambda (x) (+ (* x x) (sqrt x))))
       (cps (cps-step form))
       (analysis (analyze-cps cps)))
  (display "\nAnalysis results:\n")
  (pretty-print analysis)
  (newline))

;; Example 3: Find call sites
(display "3. Finding call sites in complex expression:\n")
(let* ((form '(lambda (x y)
                (let ((a (* x 2))
                      (b (+ y 1)))
                  (if (> a b)
                      (max a b)
                      (min a b)))))
       (cps (cps-step form))
       (call-sites (find-call-sites cps)))
  (display "\nCall sites found:\n")
  (for-each (lambda (site)
              (format #t "  Type: ~a, "
                      (assq-ref site 'type))
              (cond
               ((assq-ref site 'procedure) =>
                (lambda (proc) (format #t "Procedure: ~s, " proc)))
               ((assq-ref site 'primitive) =>
                (lambda (prim) (format #t "Primitive: ~s, " prim))))
              (format #t "Args: ~s\n" (assq-ref site 'arguments)))
            call-sites)
  (newline))

;; Example 4: Variable analysis
(display "4. Variable analysis:\n")
(let* ((form '(lambda (x y)
                (let ((z (+ x y)))
                  (* z z))))
       (cps (cps-step form))
       (var-info (analyze-variables cps)))
  (display "\nVariable information:\n")
  (format #t "  Defined: ~s\n" (assq-ref var-info 'defined))
  (format #t "  Used: ~s\n" (assq-ref var-info 'used))
  (newline))

;; Example 5: Free variables
(display "5. Free variable analysis:\n")
(let* ((form '(lambda (x) (+ x y))) ; y is free
       (cps (cps-step form))
       (free-vars (compute-free-variables cps)))
  (format #t "  Expression: (lambda (x) (+ x y))\n")
  (format #t "  Free variables: ~s\n" free-vars)
  (display "  Note: 'y' is free because it's not bound in the lambda\n")
  (newline))

(display "\nThe analysis tools help understand code structure,\n")
(display "find optimization opportunities, and detect issues.\n")