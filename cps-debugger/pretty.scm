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
  #:use-module (language cps)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (cps-pretty-print
            pretty-print-continuation
            pretty-print-expression))

;;; Commentary:
;;;
;;; Pretty-printing utilities for CPS terms.
;;;
;;; Code:

(define* (cps-pretty-print cps #:key (port (current-output-port)) (indent 0))
  "Pretty-print CPS term to PORT with INDENT."
  (define (spaces n)
    (make-string n #\space))
  
  (define (pp obj indent)
    (display (spaces indent) port)
    (match obj
      (($ $continue k src exp)
       (format port "(continue ~a" k)
       (when src
         (format port " #:src ~s" src))
       (newline port)
       (pp-expression exp (+ indent 2))
       (display (spaces indent) port)
       (display ")" port))
      
      (($ $kargs names syms body)
       (format port "(kargs ~s ~s" names syms)
       (newline port)
       (pp body (+ indent 2))
       (display (spaces indent) port)
       (display ")" port))
      
      (($ $kreceive arity k)
       (format port "(kreceive ~s ~a)" arity k))
      
      (($ $kfun src meta self tail clause)
       (format port "(kfun")
       (when src
         (format port " #:src ~s" src))
       (when meta
         (format port " #:meta ~s" meta))
       (format port " ~a ~a" self tail)
       (when clause
         (newline port)
         (pp clause (+ indent 2)))
       (display (spaces indent) port)
       (display ")" port))
      
      (_
       (format port "~s" obj))))
  
  (define (pp-expression exp indent)
    (display (spaces indent) port)
    (match exp
      (($ $const val)
       (format port "(const ~s)" val))
      
      (($ $prim name)
       (format port "(prim ~a)" name))
      
      (($ $call proc args)
       (format port "(call ~a ~s)" proc args))
      
      (($ $primcall name args)
       (format port "(primcall ~a ~s)" name args))
      
      (($ $values args)
       (format port "(values ~s)" args))
      
      (($ $branch k exp)
       (format port "(branch ~a" k)
       (newline port)
       (pp-expression exp (+ indent 2))
       (display (spaces indent) port)
       (display ")" port))
      
      (_
       (format port "~s" exp)))
    (newline port))
  
  (pp cps indent)
  (newline port))

(define* (pretty-print-continuation k cps #:key (port (current-output-port)))
  "Pretty-print continuation K from CPS."
  (let ((cont (find-continuation-in-cps k cps)))
    (if cont
        (cps-pretty-print cont #:port port)
        (format port "Continuation ~a not found~%" k))))

(define* (pretty-print-expression exp #:key (port (current-output-port)))
  "Pretty-print a CPS expression."
  (match exp
    (($ $const val)
     (format port "(const ~s)" val))
    (($ $prim name)
     (format port "(prim ~a)" name))
    (($ $call proc args)
     (format port "(call ~a ~s)" proc args))
    (($ $primcall name args)
     (format port "(primcall ~a ~s)" name args))
    (($ $values args)
     (format port "(values ~s)" args))
    (_
     (format port "~s" exp)))
  (newline port))

(define (find-continuation-in-cps k cps)
  "Helper to find continuation K in CPS."
  (match cps
    (($ $continue k* _ _) 
     (and (eq? k k*) cps))
    (($ $kargs _ _ body)
     (find-continuation-in-cps k body))
    (($ $kfun _ _ _ _ clause)
     (and clause (find-continuation-in-cps k clause)))
    (_ #f)))