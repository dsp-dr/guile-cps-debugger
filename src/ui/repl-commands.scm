;;; repl-commands.scm --- REPL integration for CPS debugger
;;; Commentary:
;;;
;;; This module provides REPL commands for interactive CPS debugging.
;;; It integrates with Guile's REPL system to provide convenient commands
;;; for tracing, profiling, and analyzing CPS code.
;;;
;;; Code:

(define-module (cps-debugger ui repl-commands)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (cps-debugger core debugger)
  #:use-module (cps-debugger analysis continuation)
  #:use-module (cps-debugger analysis memory)
  #:use-module (cps-debugger ui visualizer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (install-cps-commands
            uninstall-cps-commands))

;;; Global debugger instance
(define *cps-debugger* #f)

;;; Install commands
(define (install-cps-commands repl)
  "Install CPS debugging commands in REPL."
  
  ;; Ensure debugger is initialized
  (unless *cps-debugger*
    (set! *cps-debugger* (make-cps-debugger)))
  
  ;; Define meta commands
  (define-meta-command ((cps-trace repl) #:optional expr . args)
    "Trace CPS execution.
Usage: ,cps-trace EXPRESSION
       ,cps-trace (factorial-cps 5 identity)

Traces the execution of CPS code and displays continuation chains."
    (if expr
        (trace-cps-expression repl expr args)
        (display "Usage: ,cps-trace EXPRESSION\n")))
  
  (define-meta-command ((cps-profile repl) #:optional expr . args)
    "Profile memory usage of CPS code.
Usage: ,cps-profile EXPRESSION
       ,cps-profile (tree-sum-cps big-tree identity)

Profiles memory allocation and usage during CPS execution."
    (if expr
        (profile-cps-expression repl expr args)
        (display "Usage: ,cps-profile EXPRESSION\n")))
  
  (define-meta-command ((cps-break repl) #:optional proc)
    "Set CPS-aware breakpoint.
Usage: ,cps-break PROCEDURE
       ,cps-break factorial-cps

Sets a breakpoint that triggers when the specified procedure is called."
    (if proc
        (set-cps-breakpoint repl proc)
        (display "Usage: ,cps-break PROCEDURE\n")))
  
  (define-meta-command ((cps-chain repl))
    "Show current continuation chain.
Usage: ,cps-chain

Displays the current continuation chain from the last trace."
    (show-continuation-chain repl))
  
  (define-meta-command ((cps-memory repl))
    "Show memory statistics.
Usage: ,cps-memory

Displays memory statistics from the last profiling run."
    (show-memory-statistics repl))
  
  (define-meta-command ((cps-visualize repl) #:optional format)
    "Visualize continuation chain.
Usage: ,cps-visualize [FORMAT]
       ,cps-visualize ascii
       ,cps-visualize dot

FORMAT can be 'ascii' (default) or 'dot' for Graphviz output."
    (visualize-chain repl (or format 'ascii)))
  
  (define-meta-command ((cps-config repl) #:optional key value)
    "Configure CPS debugger.
Usage: ,cps-config                    ; Show all settings
       ,cps-config KEY               ; Show specific setting
       ,cps-config KEY VALUE         ; Set configuration value

Examples:
  ,cps-config max-trace-depth 200
  ,cps-config color-output #f"
    (cond
     ((and key value) (set-debugger-config repl key value))
     (key (show-debugger-config repl key))
     (else (show-all-config repl))))
  
  (define-meta-command ((cps-clear repl))
    "Clear CPS debugger state.
Usage: ,cps-clear

Clears all trace data and resets the debugger state."
    (clear-debugger-state repl))
  
  (define-meta-command ((cps-help repl))
    "Show CPS debugger help.
Usage: ,cps-help

Displays help for all CPS debugging commands."
    (show-cps-help repl))
  
  (format #t "CPS debugging commands installed. Type ,cps-help for help.\n"))

;;; Uninstall commands
(define (uninstall-cps-commands repl)
  "Remove CPS debugging commands from REPL."
  ;; Note: Guile doesn't provide a way to undefine meta commands
  ;; This is a placeholder for future functionality
  (format #t "CPS debugging commands cannot be uninstalled in current Guile version.\n"))

;;; Command implementations

(define (trace-cps-expression repl expr args)
  "Trace CPS expression execution."
  (let* ((full-expr (if (null? args) expr (cons expr args)))
         (thunk (lambda () (repl-eval repl full-expr))))
    
    (format #t "Tracing CPS execution...\n")
    (call-with-values
     (lambda () (debugger-trace-cps *cps-debugger* thunk))
     (lambda (result error state)
       (if error
           (begin
             (format #t "Error during execution: ~a\n" error)
             (when (debugger-get-option *cps-debugger* 'verbose)
               (format #t "Stack trace available with ,backtrace\n")))
           (begin
             (format #t "Result: ~s\n" result)
             (let* ((trace-data (state-trace-data state))
                    (chain-info (analyze-continuation-chain trace-data)))
               (format #t "\n~a\n" (format-continuation-chain chain-info))
               (when (debugger-get-option *cps-debugger* 'visualization)
                 (format #t "\nUse ,cps-visualize to see the continuation chain.\n")))))))))

(define (profile-cps-expression repl expr args)
  "Profile memory usage of CPS expression."
  (let* ((full-expr (if (null? args) expr (cons expr args)))
         (thunk (lambda () (repl-eval repl full-expr))))
    
    (format #t "Profiling CPS memory usage...\n")
    (call-with-values
     (lambda () (profile-cps-memory thunk))
     (lambda (result report)
       (format #t "Result: ~s\n\n" result)
       (display (format-memory-report report))
       (newline)))))

(define (set-cps-breakpoint repl proc)
  "Set a breakpoint on CPS procedure."
  (format #t "Setting breakpoint on ~a...\n" proc)
  ;; TODO: Implement breakpoint functionality
  (format #t "Breakpoint functionality not yet implemented.\n"))

(define (show-continuation-chain repl)
  "Display the current continuation chain."
  (let ((state (debugger-state *cps-debugger*)))
    (if state
        (let* ((trace-data (state-trace-data state))
               (chain-info (analyze-continuation-chain trace-data)))
          (display (format-continuation-chain chain-info))
          (newline))
        (format #t "No trace data available. Run ,cps-trace first.\n"))))

(define (show-memory-statistics repl)
  "Display memory statistics from last profiling."
  (format #t "Memory statistics display not yet implemented.\n"))

(define (visualize-chain repl format)
  "Visualize the continuation chain."
  (let ((state (debugger-state *cps-debugger*)))
    (if state
        (let* ((trace-data (state-trace-data state))
               (chain-info (analyze-continuation-chain trace-data)))
          (case format
            ((ascii)
             (display (ascii-tree-view chain-info))
             (newline))
            ((dot)
             (display (generate-dot-graph chain-info))
             (newline))
            (else
             (format #t "Unknown format: ~a. Use 'ascii' or 'dot'.\n" format))))
        (format #t "No trace data available. Run ,cps-trace first.\n"))))

(define (set-debugger-config repl key value)
  "Set debugger configuration option."
  (let ((key-sym (if (symbol? key) key (string->symbol key))))
    (debugger-set-option! *cps-debugger* key-sym value)
    (format #t "Set ~a to ~s\n" key-sym value)))

(define (show-debugger-config repl key)
  "Show specific debugger configuration."
  (let* ((key-sym (if (symbol? key) key (string->symbol key)))
         (value (debugger-get-option *cps-debugger* key-sym)))
    (if value
        (format #t "~a: ~s\n" key-sym value)
        (format #t "Unknown configuration option: ~a\n" key-sym))))

(define (show-all-config repl)
  "Show all debugger configuration."
  (format #t "CPS Debugger Configuration:\n")
  (let ((options (debugger-options *cps-debugger*)))
    (for-each (lambda (pair)
                (format #t "  ~a: ~s\n" (car pair) (cdr pair)))
              options)))

(define (clear-debugger-state repl)
  "Clear all debugger state."
  (debugger-reset-state *cps-debugger*)
  (format #t "Debugger state cleared.\n"))

(define (show-cps-help repl)
  "Display help for CPS debugging commands."
  (display "
CPS Debugger Commands:
=====================

,cps-trace EXPR      - Trace CPS execution and show continuation chains
,cps-profile EXPR    - Profile memory usage during CPS execution
,cps-break PROC      - Set breakpoint on procedure (not yet implemented)
,cps-chain           - Show the last traced continuation chain
,cps-memory          - Show memory statistics from last profiling
,cps-visualize [FMT] - Visualize continuation chain (ascii or dot format)
,cps-config [K] [V]  - Show/set debugger configuration
,cps-clear           - Clear debugger state
,cps-help            - Show this help message

Examples:
  ,cps-trace (factorial-cps 5 (lambda (x) x))
  ,cps-profile (fibonacci-cps 10 identity)
  ,cps-visualize dot
  ,cps-config max-trace-depth 200

For more information, see the CPS debugger documentation.
"))