#!/usr/bin/env guile
!#

;;; Main test runner for CPS debugger

(use-modules (srfi srfi-64)
             (ice-9 ftw))

;; Add current directory to load path
(add-to-load-path (dirname (current-filename)))

(define (run-test-file file)
  "Run a single test file."
  (format #t "Running ~a...~%" file)
  (load file))

(define (find-test-files dir)
  "Find all test-*.scm files in directory."
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (string-prefix? "test-" (basename filename))
                      (string-suffix? ".scm" filename))
             (set! files (cons filename files)))
           #t))
    (sort files string<?)))

(define (main args)
  "Run all tests."
  (let ((test-dir (if (null? (cdr args))
                      "tests"
                      (cadr args))))
    (format #t "CPS Debugger Test Suite~%")
    (format #t "=======================~%~%")
    
    (let ((test-files (find-test-files test-dir)))
      (format #t "Found ~a test files~%~%" (length test-files))
      
      (for-each run-test-file test-files)
      
      (format #t "~%Test Summary:~%")
      (format #t "=============~%")
      (test-runner-on-final! (test-runner-current)
                            (lambda (runner)
                              (format #t "Passed: ~a~%" (test-runner-pass-count runner))
                              (format #t "Failed: ~a~%" (test-runner-fail-count runner))
                              (format #t "Skipped: ~a~%" (test-runner-skip-count runner))
                              (exit (if (zero? (test-runner-fail-count runner)) 0 1)))))))

;; Run tests
(main (command-line))