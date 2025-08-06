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

(define-module (cps-debugger stepper)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (cps-debugger compat)
  #:use-module (cps-debugger pretty)
  #:use-module (cps-debugger inspector)
  #:export (make-stepper
            stepper?
            stepper-term
            stepper-position
            stepper-history
            stepper-breakpoints
            stepper-state
            step-forward
            step-backward
            step-into
            step-over
            step-out
            step-to-breakpoint
            add-breakpoint
            remove-breakpoint
            list-breakpoints
            stepper-current-context
            stepper-print-state))

;;; Commentary:
;;;
;;; Step-through debugging functionality for CPS terms.
;;;
;;; Code:

(define-record-type <stepper>
  (make-stepper* term position history breakpoints state)
  stepper?
  (term stepper-term set-stepper-term!)
  (position stepper-position set-stepper-position!)
  (history stepper-history set-stepper-history!)
  (breakpoints stepper-breakpoints set-stepper-breakpoints!)
  (state stepper-state set-stepper-state!))

(define* (make-stepper term #:key (breakpoints '()))
  "Create a new stepper for TERM with optional BREAKPOINTS."
  (make-stepper* term 
                 '() ; Initial position (empty path)
                 '() ; History
                 breakpoints
                 'ready))

(define (stepper-current-context stepper)
  "Get the current context at the stepper's position."
  (let ((term (stepper-term stepper))
        (pos (stepper-position stepper)))
    (navigate-to-position term pos)))

(define (navigate-to-position term path)
  "Navigate to a position in TERM specified by PATH."
  (if (null? path)
      term
      (let ((index (car path))
            (rest (cdr path)))
        (cond
         ((and (pair? term) (list? term))
          (navigate-to-position (list-ref term index) rest))
         (else term)))))

(define (step-forward stepper)
  "Step forward to the next expression."
  (let* ((term (stepper-term stepper))
         (pos (stepper-position stepper))
         (next-pos (find-next-position term pos)))
    (if next-pos
        (begin
          (set-stepper-history! stepper 
                               (cons pos (stepper-history stepper)))
          (set-stepper-position! stepper next-pos)
          (set-stepper-state! stepper 'stepped)
          stepper)
        (begin
          (set-stepper-state! stepper 'end)
          stepper))))

(define (step-backward stepper)
  "Step backward to the previous expression."
  (let ((history (stepper-history stepper)))
    (if (null? history)
        (begin
          (set-stepper-state! stepper 'start)
          stepper)
        (let ((prev-pos (car history)))
          (set-stepper-position! stepper prev-pos)
          (set-stepper-history! stepper (cdr history))
          (set-stepper-state! stepper 'stepped)
          stepper))))

(define (find-next-position term path)
  "Find the next position after PATH in TERM."
  ;; This is a simplified implementation
  ;; In a full implementation, this would traverse the CPS structure
  (cond
   ((null? path) '(0))  ; Start at first child
   ((and (pair? term) (< (car path) (- (length term) 1)))
    (cons (+ 1 (car path)) (cdr path)))
   (else #f)))

(define (step-into stepper)
  "Step into the current expression."
  (let* ((current (stepper-current-context stepper))
         (pos (stepper-position stepper)))
    (cond
     ((and (pair? current) (symbol? (car current)))
      ;; We can step into compound expressions
      (set-stepper-history! stepper 
                           (cons pos (stepper-history stepper)))
      (set-stepper-position! stepper (append pos '(0)))
      (set-stepper-state! stepper 'stepped-into)
      stepper)
     (else
      ;; Can't step into atomic expressions
      (set-stepper-state! stepper 'atomic)
      stepper))))

(define (step-over stepper)
  "Step over the current expression."
  (let* ((pos (stepper-position stepper))
         (parent-pos (if (null? pos) '() (drop-right pos 1)))
         (term (stepper-term stepper))
         (parent (navigate-to-position term parent-pos)))
    (if (and (pair? parent) 
             (< (last pos) (- (length parent) 1)))
        (begin
          (set-stepper-history! stepper 
                               (cons pos (stepper-history stepper)))
          (set-stepper-position! stepper 
                               (append parent-pos 
                                      (list (+ 1 (last pos)))))
          (set-stepper-state! stepper 'stepped-over)
          stepper)
        (step-forward stepper))))

(define (step-out stepper)
  "Step out of the current context."
  (let ((pos (stepper-position stepper)))
    (if (null? pos)
        (begin
          (set-stepper-state! stepper 'at-top)
          stepper)
        (let ((parent-pos (drop-right pos 1)))
          (set-stepper-history! stepper 
                               (cons pos (stepper-history stepper)))
          (set-stepper-position! stepper parent-pos)
          (set-stepper-state! stepper 'stepped-out)
          stepper))))

(define (add-breakpoint stepper location)
  "Add a breakpoint at LOCATION."
  (set-stepper-breakpoints! stepper 
                            (cons location (stepper-breakpoints stepper)))
  stepper)

(define (remove-breakpoint stepper location)
  "Remove breakpoint at LOCATION."
  (set-stepper-breakpoints! stepper
                            (delete location (stepper-breakpoints stepper)))
  stepper)

(define (list-breakpoints stepper)
  "List all breakpoints."
  (stepper-breakpoints stepper))

(define (step-to-breakpoint stepper)
  "Step forward until a breakpoint is reached."
  (let loop ((s stepper)
             (steps 0)
             (max-steps 1000)) ; Prevent infinite loops
    (if (>= steps max-steps)
        (begin
          (set-stepper-state! s 'max-steps-reached)
          s)
        (let* ((current-pos (stepper-position s))
               (at-breakpoint? (member current-pos 
                                      (stepper-breakpoints s))))
          (if at-breakpoint?
              (begin
                (set-stepper-state! s 'at-breakpoint)
                s)
              (let ((next (step-forward s)))
                (if (eq? (stepper-state next) 'end)
                    next
                    (loop next (+ steps 1) max-steps))))))))

(define* (stepper-print-state stepper #:key (port (current-output-port)))
  "Print the current state of the stepper."
  (format port "=== Stepper State ===~%")
  (format port "Position: ~s~%" (stepper-position stepper))
  (format port "State: ~a~%" (stepper-state stepper))
  (format port "History length: ~a~%" (length (stepper-history stepper)))
  (format port "Breakpoints: ~s~%" (stepper-breakpoints stepper))
  (format port "~%Current context:~%")
  (let ((context (stepper-current-context stepper)))
    (cps-pretty-print context #:port port #:indent 2))
  (newline port))