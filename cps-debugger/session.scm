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

(define-module (cps-debugger session)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (cps-debugger stepper)
  #:use-module (cps-debugger core)
  #:export (make-debug-session
            debug-session?
            session-id
            session-stepper
            session-debugger
            session-watches
            session-log
            session-options
            
            add-watch
            remove-watch
            list-watches
            evaluate-watches
            
            log-event
            get-session-log
            clear-session-log
            
            set-session-option
            get-session-option
            
            save-session
            load-session
            
            current-debug-session
            set-current-debug-session!
            
            session-summary))

;;; Commentary:
;;;
;;; Debug session management with state persistence and watch expressions.
;;;
;;; Code:

(define-record-type <debug-session>
  (make-debug-session* id stepper debugger watches log options)
  debug-session?
  (id session-id)
  (stepper session-stepper set-session-stepper!)
  (debugger session-debugger set-session-debugger!)
  (watches session-watches set-session-watches!)
  (log session-log set-session-log!)
  (options session-options set-session-options!))

;; Global current session
(define current-debug-session (make-parameter #f))

(define (set-current-debug-session! session)
  "Set the current debug session."
  (current-debug-session session))

(define* (make-debug-session term #:key 
                             (id (generate-session-id))
                             (watches '())
                             (options '()))
  "Create a new debug session for TERM."
  (let* ((debugger (make-cps-debugger term))
         (stepper (make-stepper term)))
    (make-debug-session* id stepper debugger watches '() options)))

(define (generate-session-id)
  "Generate a unique session ID."
  (format #f "session-~a" (current-time)))

;; Watch expressions
(define (add-watch session name expression)
  "Add a watch expression to the session."
  (let ((watches (session-watches session)))
    (set-session-watches! session 
                          (cons (cons name expression) watches))
    (log-event session 'watch-added name)
    session))

(define (remove-watch session name)
  "Remove a watch expression from the session."
  (let ((watches (session-watches session)))
    (set-session-watches! session
                          (remove (lambda (w) (eq? (car w) name))
                                  watches))
    (log-event session 'watch-removed name)
    session))

(define (list-watches session)
  "List all watch expressions."
  (session-watches session))

(define (evaluate-watches session)
  "Evaluate all watch expressions in the current context."
  (let* ((stepper (session-stepper session))
         (context (stepper-current-context stepper))
         (watches (session-watches session)))
    (map (lambda (watch)
           (cons (car watch)
                 (evaluate-in-context (cdr watch) context)))
         watches)))

(define (evaluate-in-context expr context)
  "Evaluate EXPR in the given CONTEXT."
  ;; Simplified evaluation - in a full implementation,
  ;; this would properly evaluate in the CPS context
  (catch #t
    (lambda () 
      (if (procedure? expr)
          (expr context)
          expr))
    (lambda args
      (format #f "<error: ~a>" (cadr args)))))

;; Logging
(define (log-event session event-type . data)
  "Log an event in the session."
  (let* ((timestamp (current-time))
         (entry (list timestamp event-type data))
         (log (session-log session)))
    (set-session-log! session (cons entry log))
    session))

(define (get-session-log session)
  "Get the session log."
  (reverse (session-log session)))

(define (clear-session-log session)
  "Clear the session log."
  (set-session-log! session '())
  (log-event session 'log-cleared)
  session)

;; Options
(define (set-session-option session key value)
  "Set a session option."
  (let ((options (session-options session)))
    (set-session-options! session
                          (assoc-set! options key value))
    (log-event session 'option-set key value)
    session))

(define (get-session-option session key . default)
  "Get a session option value."
  (let ((options (session-options session)))
    (or (assoc-ref options key)
        (and (pair? default) (car default)))))

;; Persistence
(define (save-session session filename)
  "Save session state to a file."
  (call-with-output-file filename
    (lambda (port)
      (write `((id . ,(session-id session))
               (position . ,(stepper-position (session-stepper session)))
               (breakpoints . ,(stepper-breakpoints (session-stepper session)))
               (watches . ,(session-watches session))
               (options . ,(session-options session)))
             port)))
  (log-event session 'session-saved filename)
  session)

(define (load-session filename term)
  "Load a session from a file."
  (let ((data (call-with-input-file filename read)))
    (let* ((id (assoc-ref data 'id))
           (position (assoc-ref data 'position))
           (breakpoints (assoc-ref data 'breakpoints))
           (watches (assoc-ref data 'watches))
           (options (assoc-ref data 'options))
           (session (make-debug-session term 
                                       #:id id
                                       #:watches watches
                                       #:options options)))
      ;; Restore stepper state
      (let ((stepper (session-stepper session)))
        (set-stepper-position! stepper position)
        (set-stepper-breakpoints! stepper breakpoints))
      (log-event session 'session-loaded filename)
      session)))

;; Session summary
(define* (session-summary session #:key (port (current-output-port)))
  "Print a summary of the session."
  (format port "=== Debug Session Summary ===~%")
  (format port "ID: ~a~%" (session-id session))
  (format port "~%Stepper:~%")
  (let ((stepper (session-stepper session)))
    (format port "  Position: ~s~%" (stepper-position stepper))
    (format port "  State: ~a~%" (stepper-state stepper))
    (format port "  Breakpoints: ~a~%" 
            (length (stepper-breakpoints stepper))))
  (format port "~%Watches: ~a~%" (length (session-watches session)))
  (for-each (lambda (watch)
              (format port "  ~a: ~s~%" (car watch) (cdr watch)))
            (session-watches session))
  (format port "~%Log entries: ~a~%" (length (session-log session)))
  (format port "~%Options:~%")
  (for-each (lambda (opt)
              (format port "  ~a: ~s~%" (car opt) (cdr opt)))
            (session-options session))
  (newline port))