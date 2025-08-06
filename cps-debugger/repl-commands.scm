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

(define-module (cps-debugger repl-commands)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (cps-debugger core)
  #:use-module (cps-debugger pretty)
  #:use-module (cps-debugger inspector)
  #:use-module (cps-debugger stepper)
  #:use-module (cps-debugger analysis)
  #:use-module (ice-9 format)
  #:export (install-cps-commands!
            current-stepper
            set-current-stepper!))

;;; Commentary:
;;;
;;; Enhanced REPL integration for the CPS debugger.
;;;
;;; Code:

;; Global stepper for REPL commands
(define current-stepper (make-parameter #f))

(define (set-current-stepper! stepper)
  "Set the current stepper for REPL commands."
  (current-stepper stepper))

(define (ensure-stepper)
  "Ensure we have a current stepper."
  (or (current-stepper)
      (error "No active stepper. Use ,cps-debug to start debugging.")))

(define (install-cps-commands!)
  "Install CPS debugging commands in the REPL."
  
  ;; Basic commands (already exist, enhancing them)
  (define-meta-command ((cps-debug guile) repl form)
    "cps-debug FORM
Debug the CPS compilation of FORM."
    (let* ((debugger (cps-debug form))
           (term (debugger-term debugger))
           (stepper (make-stepper term)))
      (set-current-stepper! stepper)
      (format #t "CPS debugger started.~%")
      (format #t "Current stepper position: ~s~%" 
              (stepper-position stepper))
      (format #t "Use ,step to step through, ,inspect to inspect.~%")))
  
  (define-meta-command ((cps-step guile) repl form)
    "cps-step FORM
Step through CPS transformation of FORM."
    (let* ((cps (cps-step form))
           (stepper (make-stepper cps)))
      (set-current-stepper! stepper)
      (stepper-print-state stepper)))
  
  ;; Stepping commands
  (define-meta-command ((step guile) repl)
    "step
Step forward to the next expression."
    (let ((stepper (ensure-stepper)))
      (step-forward stepper)
      (stepper-print-state stepper)))
  
  (define-meta-command ((step-back guile) repl)
    "step-back
Step backward to the previous expression."
    (let ((stepper (ensure-stepper)))
      (step-backward stepper)
      (stepper-print-state stepper)))
  
  (define-meta-command ((step-into guile) repl)
    "step-into
Step into the current expression."
    (let ((stepper (ensure-stepper)))
      (step-into stepper)
      (stepper-print-state stepper)))
  
  (define-meta-command ((step-over guile) repl)
    "step-over
Step over the current expression."
    (let ((stepper (ensure-stepper)))
      (step-over stepper)
      (stepper-print-state stepper)))
  
  (define-meta-command ((step-out guile) repl)
    "step-out
Step out of the current context."
    (let ((stepper (ensure-stepper)))
      (step-out stepper)
      (stepper-print-state stepper)))
  
  ;; Breakpoint commands
  (define-meta-command ((break guile) repl location)
    "break LOCATION
Add a breakpoint at LOCATION (a position path)."
    (let ((stepper (ensure-stepper)))
      (add-breakpoint stepper location)
      (format #t "Breakpoint added at ~s~%" location)
      (format #t "Current breakpoints: ~s~%" 
              (list-breakpoints stepper))))
  
  (define-meta-command ((unbreak guile) repl location)
    "unbreak LOCATION
Remove breakpoint at LOCATION."
    (let ((stepper (ensure-stepper)))
      (remove-breakpoint stepper location)
      (format #t "Breakpoint removed from ~s~%" location)
      (format #t "Current breakpoints: ~s~%" 
              (list-breakpoints stepper))))
  
  (define-meta-command ((breakpoints guile) repl)
    "breakpoints
List all breakpoints."
    (let ((stepper (ensure-stepper)))
      (let ((bps (list-breakpoints stepper)))
        (if (null? bps)
            (format #t "No breakpoints set.~%")
            (begin
              (format #t "Breakpoints:~%")
              (for-each (lambda (bp)
                          (format #t "  ~s~%" bp))
                        bps))))))
  
  (define-meta-command ((continue guile) repl)
    "continue
Continue execution to the next breakpoint."
    (let ((stepper (ensure-stepper)))
      (step-to-breakpoint stepper)
      (stepper-print-state stepper)))
  
  ;; Inspection commands
  (define-meta-command ((cps-inspect guile) repl . args)
    "cps-inspect [FORM]
Inspect the current CPS position or FORM."
    (cond
     ((null? args)
      ;; Inspect current position
      (let* ((stepper (ensure-stepper))
             (context (stepper-current-context stepper))
             (info (cps-inspect context)))
        (format #t "~%CPS inspection at position ~s:~%" 
                (stepper-position stepper))
        (for-each (lambda (pair)
                    (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                  info)))
     (else
      ;; Inspect given form
      (let* ((form (car args))
             (cps (cps-step form))
             (info (cps-inspect cps)))
        (format #t "~%CPS inspection:~%")
        (for-each (lambda (pair)
                    (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                  info)))))
  
  (define-meta-command ((cps-pretty guile) repl . args)
    "cps-pretty [FORM]
Pretty-print the current CPS position or FORM."
    (cond
     ((null? args)
      ;; Pretty-print current position
      (let* ((stepper (ensure-stepper))
             (context (stepper-current-context stepper)))
        (format #t "~%Pretty-printed CPS at position ~s:~%" 
                (stepper-position stepper))
        (cps-pretty-print context)))
     (else
      ;; Pretty-print given form
      (let ((cps (cps-step (car args))))
        (format #t "~%Pretty-printed CPS:~%")
        (cps-pretty-print cps)))))
  
  ;; Analysis commands
  (define-meta-command ((cps-analyze guile) repl . args)
    "cps-analyze [FORM]
Analyze the current CPS position or FORM."
    (let* ((cps (if (null? args)
                    (stepper-current-context (ensure-stepper))
                    (cps-step (car args))))
           (analysis (analyze-cps cps)))
      (format #t "~%CPS Analysis:~%")
      (for-each (lambda (pair)
                  (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                analysis)))
  
  ;; State command
  (define-meta-command ((cps-state guile) repl)
    "cps-state
Show the current debugger state."
    (let ((stepper (current-stepper)))
      (if stepper
          (stepper-print-state stepper)
          (format #t "No active debugger session.~%"))))
  
  ;; Reset command
  (define-meta-command ((cps-reset guile) repl)
    "cps-reset
Reset the debugger to the beginning."
    (let ((stepper (ensure-stepper)))
      (set-stepper-position! stepper '())
      (set-stepper-history! stepper '())
      (set-stepper-state! stepper 'ready)
      (format #t "Debugger reset to beginning.~%")
      (stepper-print-state stepper)))
  
  'installed)

;; Auto-install commands when module is loaded
(install-cps-commands!)