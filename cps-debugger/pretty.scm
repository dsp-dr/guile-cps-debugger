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

(define-module (cps-debugger pretty)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (cps-debugger compat)
  #:export (cps-pretty-print
            pretty-print-continuation
            pretty-print-expression))

;;; Commentary:
;;;
;;; Pretty-printing utilities for CPS terms.
;;;
;;; Code:

(define* (cps-pretty-print cps #:key (port (current-output-port)) (indent 0))
  "Pretty-print CPS or pseudo-CPS term to PORT with INDENT."
  (define (spaces n)
    (make-string n #\space))
  
  (define (pp obj indent)
    (display (spaces indent) port)
    (cond
     ;; Handle pseudo-CPS structures
     ((and (pair? obj) (symbol? (car obj)))
      (match obj
        (('lambda meta body)
         (format port "(lambda ~s" meta)
         (newline port)
         (pp body (+ indent 2))
         (display (spaces indent) port)
         (display ")" port))
        
        (('lambda-case args gensyms body . alt)
         (format port "(lambda-case ~s ~s" args gensyms)
         (newline port)
         (pp body (+ indent 2))
         (when (pair? alt)
           (newline port)
           (pp (car alt) (+ indent 2)))
         (display (spaces indent) port)
         (display ")" port))
        
        (('call proc . args)
         (format port "(call ")
         (pp proc 0)
         (for-each (lambda (arg)
                     (display " " port)
                     (pp arg 0))
                   args)
         (display ")" port))
        
        (('primcall name . args)
         (format port "(primcall ~a" name)
         (for-each (lambda (arg)
                     (display " " port)
                     (pp arg 0))
                   args)
         (display ")" port))
        
        (('if test then else)
         (format port "(if")
         (newline port)
         (pp test (+ indent 2))
         (newline port)
         (pp then (+ indent 2))
         (newline port)
         (pp else (+ indent 2))
         (display (spaces indent) port)
         (display ")" port))
        
        (('const val)
         (format port "(const ~s)" val))
        
        (('let bindings body)
         (format port "(let ~s" bindings)
         (newline port)
         (pp body (+ indent 2))
         (display (spaces indent) port)
         (display ")" port))
        
        (('begin . exps)
         (format port "(begin")
         (for-each (lambda (exp)
                     (newline port)
                     (pp exp (+ indent 2)))
                   exps)
         (display (spaces indent) port)
         (display ")" port))
        
        (_
         (pretty-print obj port))))
     
     ;; Handle symbols
     ((symbol? obj)
      (display obj port))
     
     ;; Fallback
     (else
      (pretty-print obj port))))
  
  (pp cps indent)
  (newline port))

(define* (pretty-print-continuation k cps #:key (port (current-output-port)))
  "Pretty-print continuation K from CPS (compatibility stub)."
  (format port "Continuation ~a in:~%" k)
  (cps-pretty-print cps #:port port #:indent 2))

(define* (pretty-print-expression exp #:key (port (current-output-port)))
  "Pretty-print a CPS expression."
  (cps-pretty-print exp #:port port))