#!/usr/bin/env guile
!#

;;; Test suite for cps-debugger analysis functionality

(use-modules (cps-debugger analysis)
             (cps-debugger compat)
             (srfi srfi-64))

(test-begin "cps-debugger-analysis")

(test-assert "analyze-cps returns expected structure"
  (let ((result (analyze-cps '(lambda () (const 42)))))
    (and (assq 'node-count result)
         (assq 'call-sites result)
         (assq 'variable-info result)
         (assq 'free-variables result))))

(test-assert "count-nodes counts simple lambda"
  (let ((count (count-nodes '(lambda () (const 42)))))
    (> count 0)))

(test-assert "count-nodes counts call"
  (let ((count (count-nodes '(call + x y))))
    (>= count 4))) ; call, +, x, y

(test-assert "find-call-sites finds calls"
  (let ((sites (find-call-sites '(call + x y))))
    (and (pair? sites)
         (assq 'type (car sites))
         (eq? (assq-ref (car sites) 'type) 'call))))

(test-assert "find-call-sites finds primcalls"
  (let ((sites (find-call-sites '(primcall + x y))))
    (and (pair? sites)
         (assq 'type (car sites))
         (eq? (assq-ref (car sites) 'type) 'primcall))))

(test-assert "analyze-variables tracks defined variables"
  (let* ((info (analyze-variables '(lambda-case ((x) #f #f #f) (x) x)))
         (defined (assq-ref info 'defined)))
    (member 'x defined)))

(test-assert "analyze-variables tracks used variables"
  (let* ((info (analyze-variables '(call + x y)))
         (used (assq-ref info 'used)))
    (and (member '+ used)
         (member 'x used)
         (member 'y used))))

(test-assert "compute-free-variables finds free vars"
  (let ((free (compute-free-variables '(call + x y))))
    (and (member '+ free)
         (member 'x free)
         (member 'y free))))

(test-assert "compute-free-variables excludes bound vars"
  (let ((free (compute-free-variables 
                '(lambda-case ((x) #f #f #f) (x) (call + x y)))))
    (and (member '+ free)
         (member 'y free)
         (not (member 'x free)))))

(test-end "cps-debugger-analysis")