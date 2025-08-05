;;; debugger.scm --- Core debugging engine for CPS code analysis
;;; Commentary:
;;;
;;; This module provides the main debugging engine for analyzing and debugging
;;; Continuation-Passing Style (CPS) code in Guile Scheme. It integrates with
;;; Guile's VM to provide hooks for tracing, profiling, and analyzing CPS
;;; execution patterns.
;;;
;;; Code:

(define-module (cps-debugger core debugger)
  #:use-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm trap-state)
  #:use-module (system vm debug)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-cps-debugger
            cps-debugger?
            debugger-install-hooks
            debugger-uninstall-hooks
            debugger-trace-cps
            debugger-profile-memory
            debugger-set-option!
            debugger-get-option
            debugger-reset-state))

;;; Debugger state record type
(define-record-type <cps-debugger-state>
  (make-debugger-state trace-data profile-data breakpoints)
  debugger-state?
  (trace-data state-trace-data set-state-trace-data!)
  (profile-data state-profile-data set-state-profile-data!)
  (breakpoints state-breakpoints set-state-breakpoints!))

;;; Main debugger record type
(define-record-type <cps-debugger>
  (make-cps-debugger-internal vm options state hooks)
  cps-debugger?
  (vm debugger-vm)
  (options debugger-options set-debugger-options!)
  (state debugger-state set-debugger-state!)
  (hooks debugger-hooks set-debugger-hooks!))

;;; Constructor with defaults
(define* (make-cps-debugger #:key
                           (vm (the-vm))
                           (options (make-default-options)))
  "Create a new CPS debugger instance with the specified VM and options."
  (let ((state (make-debugger-state '() '() '())))
    (make-cps-debugger-internal vm options state '())))

;;; Default options
(define (make-default-options)
  '((max-trace-depth . 100)
    (memory-profiling . #t)
    (visualization . #t)
    (output-format . repl)
    (trace-continuations . #t)
    (profile-closures . #t)
    (break-on-error . #t)))

;;; Option management
(define (debugger-set-option! debugger key value)
  "Set a debugger option."
  (let ((options (debugger-options debugger)))
    (set-debugger-options! debugger (assq-set! options key value))))

(define (debugger-get-option debugger key)
  "Get a debugger option value."
  (assq-ref (debugger-options debugger) key))

;;; State management
(define (debugger-reset-state debugger)
  "Reset the debugger state."
  (set-debugger-state! debugger (make-debugger-state '() '() '())))

;;; Hook installation
(define (debugger-install-hooks debugger)
  "Install VM hooks for CPS debugging."
  (let ((vm (debugger-vm debugger)))
    ;; Install call hook for tracing
    (when (debugger-get-option debugger 'trace-continuations)
      (add-hook! (vm-apply-hook vm) 
                 (make-trace-hook debugger)))
    
    ;; Install memory profiling hook
    (when (debugger-get-option debugger 'memory-profiling)
      (add-hook! (vm-push-continuation-hook vm)
                 (make-memory-hook debugger)))
    
    ;; Store hooks for later removal
    (set-debugger-hooks! debugger 
                        (list (cons 'trace (make-trace-hook debugger))
                              (cons 'memory (make-memory-hook debugger))))))

(define (debugger-uninstall-hooks debugger)
  "Remove installed VM hooks."
  (let ((vm (debugger-vm debugger))
        (hooks (debugger-hooks debugger)))
    (for-each (lambda (hook-pair)
                (match hook-pair
                  ((name . hook)
                   (case name
                     ((trace) (remove-hook! (vm-apply-hook vm) hook))
                     ((memory) (remove-hook! (vm-push-continuation-hook vm) hook))))))
              hooks)
    (set-debugger-hooks! debugger '())))

;;; Trace hook implementation
(define (make-trace-hook debugger)
  "Create a trace hook for CPS calls."
  (lambda (frame)
    (let* ((proc (frame-procedure frame))
           (args (frame-arguments frame))
           (state (debugger-state debugger))
           (trace-data (state-trace-data state)))
      ;; Check if this looks like a CPS call (has continuation argument)
      (when (and (procedure? proc)
                 (not (null? args))
                 (procedure? (last args)))
        (let ((entry (make-trace-entry proc args frame)))
          (set-state-trace-data! state (cons entry trace-data)))))))

;;; Memory profiling hook
(define (make-memory-hook debugger)
  "Create a memory profiling hook."
  (lambda (frame cont)
    (let* ((state (debugger-state debugger))
           (profile-data (state-profile-data state))
           (memory-info (capture-memory-info frame cont)))
      (set-state-profile-data! state (cons memory-info profile-data)))))

;;; Trace entry creation
(define (make-trace-entry proc args frame)
  "Create a trace entry for a CPS call."
  `((procedure . ,proc)
    (arguments . ,args)
    (time . ,(current-time))
    (frame-depth . ,(frame-previous frame))))

;;; Memory info capture
(define (capture-memory-info frame cont)
  "Capture memory information for profiling."
  (let ((gc-stats (gc-stats)))
    `((frame . ,frame)
      (continuation . ,cont)
      (heap-size . ,(assq-ref gc-stats 'heap-size))
      (bytes-allocated . ,(assq-ref gc-stats 'bytes-allocated))
      (time . ,(current-time)))))

;;; Main tracing function
(define (debugger-trace-cps debugger thunk)
  "Trace CPS execution of THUNK."
  (debugger-reset-state debugger)
  (debugger-install-hooks debugger)
  
  (let ((result #f)
        (error #f))
    (catch #t
      (lambda ()
        (set! result (thunk)))
      (lambda args
        (set! error args)))
    
    (debugger-uninstall-hooks debugger)
    
    (if error
        (values #f error (debugger-state debugger))
        (values result #f (debugger-state debugger)))))

;;; Memory profiling function
(define (debugger-profile-memory debugger thunk)
  "Profile memory usage during CPS execution."
  (let ((initial-gc (gc-stats)))
    (debugger-reset-state debugger)
    (debugger-install-hooks debugger)
    
    (gc) ; Force GC before profiling
    
    (let ((result #f)
          (error #f))
      (catch #t
        (lambda ()
          (set! result (thunk)))
        (lambda args
          (set! error args)))
      
      (gc) ; Force GC after execution
      (let ((final-gc (gc-stats)))
        (debugger-uninstall-hooks debugger)
        
        (values (if error #f result)
                error
                (make-memory-report initial-gc final-gc 
                                  (state-profile-data (debugger-state debugger))))))))

;;; Memory report generation
(define (make-memory-report initial-gc final-gc profile-data)
  "Generate a memory usage report."
  `((initial-heap . ,(assq-ref initial-gc 'heap-size))
    (final-heap . ,(assq-ref final-gc 'heap-size))
    (heap-growth . ,(- (assq-ref final-gc 'heap-size)
                      (assq-ref initial-gc 'heap-size)))
    (total-allocated . ,(- (assq-ref final-gc 'bytes-allocated)
                          (assq-ref initial-gc 'bytes-allocated)))
    (gc-count . ,(- (assq-ref final-gc 'gc-times)
                   (assq-ref initial-gc 'gc-times)))
    (profile-entries . ,(length profile-data))
    (profile-data . ,profile-data)))

;;; Utility function
(define (last lst)
  "Return the last element of LST."
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))