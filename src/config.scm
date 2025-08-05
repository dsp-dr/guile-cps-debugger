;;; config.scm --- Configuration management for CPS debugger
;;; Commentary:
;;;
;;; This module provides configuration management facilities for the CPS
;;; debugger, including loading and saving configurations, and managing
;;; default settings.
;;;
;;; Code:

(define-module (cps-debugger config)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-26)
  #:export (default-config
            load-config
            save-config
            merge-config
            validate-config
            config-ref
            config-set))

(define default-config
  '((max-trace-depth . 100)
    (memory-profiling . #t)
    (visualization . #t)
    (output-format . repl)
    (trace-continuations . #t)
    (profile-closures . #t)
    (break-on-error . #t)
    (continuation-chain-limit . 50)
    (memory-snapshot-interval . 1000)
    (verbose . #f)
    (color-output . #t)
    (save-history . #t)
    (history-file . "~/.cps-debugger-history")))

(define (config-ref config key)
  "Get configuration value for KEY from CONFIG."
  (assq-ref config key))

(define (config-set config key value)
  "Set configuration KEY to VALUE in CONFIG."
  (assq-set! config key value))

(define (validate-config config)
  "Validate configuration and return validated config or #f."
  (define (valid-option? key value)
    (case key
      ((max-trace-depth continuation-chain-limit memory-snapshot-interval)
       (and (integer? value) (positive? value)))
      ((memory-profiling visualization trace-continuations 
        profile-closures break-on-error verbose color-output save-history)
       (boolean? value))
      ((output-format)
       (memq value '(repl json sexp ascii graphviz)))
      ((history-file)
       (string? value))
      (else #t)))
  
  (let loop ((cfg config)
             (valid '()))
    (if (null? cfg)
        valid
        (let* ((entry (car cfg))
               (key (car entry))
               (value (cdr entry)))
          (if (valid-option? key value)
              (loop (cdr cfg) (cons entry valid))
              (begin
                (format #t "Warning: Invalid configuration value for ~a: ~a~%" 
                        key value)
                (loop (cdr cfg) valid)))))))

(define (merge-config base override)
  "Merge OVERRIDE configuration into BASE configuration."
  (let loop ((override override)
             (result base))
    (if (null? override)
        result
        (let* ((entry (car override))
               (key (car entry))
               (value (cdr entry)))
          (loop (cdr override)
                (assq-set! result key value))))))

(define* (load-config #:optional (filename #f))
  "Load configuration from FILENAME or default location."
  (let ((config-file (or filename
                        (string-append (getenv "HOME") "/.cps-debugger.conf"))))
    (if (file-exists? config-file)
        (catch #t
          (lambda ()
            (call-with-input-file config-file
              (lambda (port)
                (let ((config (read port)))
                  (if (and (list? config)
                           (every pair? config))
                      (validate-config (merge-config default-config config))
                      (begin
                        (format #t "Warning: Invalid config file format~%")
                        default-config))))))
          (lambda (key . args)
            (format #t "Error loading config: ~a~%" args)
            default-config))
        default-config)))

(define* (save-config config #:optional (filename #f))
  "Save CONFIG to FILENAME or default location."
  (let ((config-file (or filename
                        (string-append (getenv "HOME") "/.cps-debugger.conf"))))
    (catch #t
      (lambda ()
        (call-with-output-file config-file
          (lambda (port)
            (pretty-print config port)
            #t)))
      (lambda (key . args)
        (format #t "Error saving config: ~a~%" args)
        #f))))