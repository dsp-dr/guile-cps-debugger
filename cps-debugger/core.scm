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

(define-module (cps-debugger core)
  #:use-module (language tree-il)
  #:use-module (language cps)
  #:use-module (language cps compile-bytecode)
  #:use-module (system base compile)
  #:use-module (ice-9 match)
  #:export (cps-debug
            cps-step
            make-cps-debugger
            debugger-step
            debugger-continue
            debugger-break))

;;; Commentary:
;;;
;;; Core debugging engine for CPS terms.
;;;
;;; Code:

(define-record-type <cps-debugger>
  (make-cps-debugger* term breakpoints stepping?)
  cps-debugger?
  (term debugger-term set-debugger-term!)
  (breakpoints debugger-breakpoints set-debugger-breakpoints!)
  (stepping? debugger-stepping? set-debugger-stepping!))

(define (make-cps-debugger term)
  "Create a new CPS debugger for TERM."
  (make-cps-debugger* term '() #f))

(define (cps-debug proc)
  "Debug the CPS compilation of PROC."
  (let* ((tree-il (compile proc #:to 'tree-il))
         (cps (compile tree-il #:from 'tree-il #:to 'cps)))
    (format #t "~%Debugging CPS for procedure~%")
    (make-cps-debugger cps)))

(define (cps-step form)
  "Step through CPS transformation of FORM."
  (let* ((tree-il (compile form #:to 'tree-il))
         (cps (compile tree-il #:from 'tree-il #:to 'cps)))
    (format #t "~%Stepping through CPS transformation~%")
    (format #t "Original form: ~s~%" form)
    (format #t "Tree-IL: ~s~%" tree-il)
    (format #t "CPS: ~s~%" cps)
    cps))

(define (debugger-step debugger)
  "Single step through the debugger."
  (set-debugger-stepping! debugger #t)
  debugger)

(define (debugger-continue debugger)
  "Continue execution in the debugger."
  (set-debugger-stepping! debugger #f)
  debugger)

(define (debugger-break debugger location)
  "Set a breakpoint at LOCATION."
  (set-debugger-breakpoints! 
   debugger 
   (cons location (debugger-breakpoints debugger)))
  debugger)