;;; guile-cps-debugger --- CPS Debugger for GNU Guile
;;; Copyright (C) 2025 dsp-dr
;;;
;;; This file is part of guile-cps-debugger.
;;;
;;; guile-cps-debugger is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; guile-cps-debugger is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guile-cps-debugger.  If not, see <https://www.gnu.org/licenses/>.

(define-module (cps-debugger cli)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (cps-debugger core)
  #:use-module (cps-debugger stepper)
  #:use-module (cps-debugger session)
  #:use-module (cps-debugger pretty)
  #:use-module (cps-debugger inspector)
  #:use-module (cps-debugger analysis)
  #:export (start-debugger-cli
            debugger-repl))

;;; Commentary:
;;;
;;; Command-line interface for interactive debugging.
;;;
;;; Code:

(define (start-debugger-cli term)
  "Start the interactive debugger CLI for TERM."
  (activate-readline)
  (let ((session (make-debug-session term)))
    (set-current-debug-session! session)
    (display-welcome)
    (debugger-repl session)))

(define (display-welcome)
  "Display welcome message."
  (display "=== Guile CPS Debugger CLI ===\n")
  (display "Type 'help' for available commands.\n")
  (display "Type 'quit' to exit.\n\n"))

(define (debugger-repl session)
  "Main REPL loop for the debugger."
  (let loop ()
    (display "cps-debug> ")
    (force-output)
    (let ((input (readline)))
      (cond
       ((eof-object? input)
        (newline)
        (display "Exiting debugger.\n"))
       ((string=? input "quit")
        (display "Exiting debugger.\n"))
       ((string=? input "")
        (loop))
       (else
        (catch #t
          (lambda ()
            (process-command session input))
          (lambda (key . args)
            (format #t "Error: ~a~%" args)))
        (loop))))))

(define (process-command session input)
  "Process a debugger command."
  (let ((parts (string-split input #\space)))
    (match parts
      (("help" . _)
       (display-help))
      
      (("step" . _)
       (let ((stepper (session-stepper session)))
         (step-forward stepper)
         (display-current-state stepper)))
      
      (("back" . _)
       (let ((stepper (session-stepper session)))
         (step-backward stepper)
         (display-current-state stepper)))
      
      (("into" . _)
       (let ((stepper (session-stepper session)))
         (step-into stepper)
         (display-current-state stepper)))
      
      (("over" . _)
       (let ((stepper (session-stepper session)))
         (step-over stepper)
         (display-current-state stepper)))
      
      (("out" . _)
       (let ((stepper (session-stepper session)))
         (step-out stepper)
         (display-current-state stepper)))
      
      (("break" pos)
       (let ((stepper (session-stepper session))
             (position (read-from-string pos)))
         (add-breakpoint stepper position)
         (format #t "Breakpoint added at ~s~%" position)))
      
      (("unbreak" pos)
       (let ((stepper (session-stepper session))
             (position (read-from-string pos)))
         (remove-breakpoint stepper position)
         (format #t "Breakpoint removed from ~s~%" position)))
      
      (("breakpoints" . _)
       (let ((stepper (session-stepper session)))
         (display-breakpoints stepper)))
      
      (("continue" . _)
       (let ((stepper (session-stepper session)))
         (step-to-breakpoint stepper)
         (display-current-state stepper)))
      
      (("watch" name expr . rest)
       (let ((expression (string-join (cons expr rest) " ")))
         (add-watch session (string->symbol name) 
                   (read-from-string expression))
         (format #t "Watch '~a' added.~%" name)))
      
      (("unwatch" name)
       (remove-watch session (string->symbol name))
       (format #t "Watch '~a' removed.~%" name))
      
      (("watches" . _)
       (display-watches session))
      
      (("inspect" . _)
       (let* ((stepper (session-stepper session))
              (context (stepper-current-context stepper))
              (info (cps-inspect context)))
         (display "\nInspection result:\n")
         (for-each (lambda (pair)
                     (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                   info)))
      
      (("pretty" . _)
       (let* ((stepper (session-stepper session))
              (context (stepper-current-context stepper)))
         (display "\nPretty-printed:\n")
         (cps-pretty-print context)))
      
      (("analyze" . _)
       (let* ((stepper (session-stepper session))
              (context (stepper-current-context stepper))
              (analysis (analyze-cps context)))
         (display "\nAnalysis:\n")
         (for-each (lambda (pair)
                     (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                   analysis)))
      
      (("where" . _)
       (let ((stepper (session-stepper session)))
         (format #t "Position: ~s~%" (stepper-position stepper))
         (format #t "State: ~a~%" (stepper-state stepper))))
      
      (("reset" . _)
       (let ((stepper (session-stepper session)))
         (set-stepper-position! stepper '())
         (set-stepper-history! stepper '())
         (set-stepper-state! stepper 'ready)
         (display "Debugger reset.\n")))
      
      (("save" filename)
       (save-session session filename)
       (format #t "Session saved to ~a~%" filename))
      
      (("load" filename)
       (let* ((term (stepper-term (session-stepper session)))
              (new-session (load-session filename term)))
         (set-current-debug-session! new-session)
         (format #t "Session loaded from ~a~%" filename)
         new-session))
      
      (("log" . _)
       (display-log session))
      
      (("summary" . _)
       (session-summary session))
      
      (_
       (format #t "Unknown command: ~a~%" input)
       (display "Type 'help' for available commands.\n")))))

(define (display-help)
  "Display help message."
  (display "
Available commands:
  
Navigation:
  step          - Step forward to next expression
  back          - Step backward to previous expression
  into          - Step into current expression
  over          - Step over current expression
  out           - Step out of current context
  
Breakpoints:
  break POS     - Add breakpoint at position
  unbreak POS   - Remove breakpoint at position
  breakpoints   - List all breakpoints
  continue      - Continue to next breakpoint
  
Inspection:
  inspect       - Inspect current context
  pretty        - Pretty-print current context
  analyze       - Analyze current context
  where         - Show current position
  
Watches:
  watch NAME EXPR - Add watch expression
  unwatch NAME    - Remove watch
  watches         - Show all watches
  
Session:
  save FILE     - Save session to file
  load FILE     - Load session from file
  log           - Show session log
  summary       - Show session summary
  reset         - Reset debugger
  
Other:
  help          - Show this help
  quit          - Exit debugger
"))

(define (display-current-state stepper)
  "Display the current state of the stepper."
  (format #t "Position: ~s~%" (stepper-position stepper))
  (format #t "State: ~a~%" (stepper-state stepper))
  (display "\nCurrent context:\n")
  (cps-pretty-print (stepper-current-context stepper) 
                    #:indent 2))

(define (display-breakpoints stepper)
  "Display all breakpoints."
  (let ((bps (list-breakpoints stepper)))
    (if (null? bps)
        (display "No breakpoints set.\n")
        (begin
          (display "Breakpoints:\n")
          (for-each (lambda (bp)
                      (format #t "  ~s~%" bp))
                    bps)))))

(define (display-watches session)
  "Display all watches and their values."
  (let ((watches (evaluate-watches session)))
    (if (null? watches)
        (display "No watches set.\n")
        (begin
          (display "Watches:\n")
          (for-each (lambda (watch)
                      (format #t "  ~a = ~s~%" 
                              (car watch) (cdr watch)))
                    watches)))))

(define (display-log session)
  "Display the session log."
  (let ((log (get-session-log session)))
    (if (null? log)
        (display "Log is empty.\n")
        (begin
          (display "Session log:\n")
          (for-each (lambda (entry)
                      (match entry
                        ((timestamp type data)
                         (format #t "  [~a] ~a ~s~%" 
                                 timestamp type data))))
                    log)))))

(define (read-from-string str)
  "Read a Scheme expression from a string."
  (call-with-input-string str read))