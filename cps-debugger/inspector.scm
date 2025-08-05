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

(define-module (cps-debugger inspector)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (ice-9 match)
  #:export (cps-inspect
            inspect-continuation
            inspect-term
            list-continuations
            find-continuation))

;;; Commentary:
;;;
;;; CPS term inspection utilities.
;;;
;;; Code:

(define (cps-inspect cps)
  "Inspect a CPS term, returning information about its structure."
  (match cps
    (($ $continue k src exp)
     `((type . continue)
       (continuation . ,k)
       (source . ,src)
       (expression . ,(inspect-expression exp))))
    (($ $kargs names syms body)
     `((type . kargs)
       (names . ,names)
       (symbols . ,syms)
       (body . ,(cps-inspect body))))
    (($ $kreceive arity k)
     `((type . kreceive)
       (arity . ,arity)
       (continuation . ,k)))
    (($ $kfun src meta self tail clause)
     `((type . kfun)
       (source . ,src)
       (meta . ,meta)
       (self . ,self)
       (tail . ,tail)
       (clause . ,(and clause (cps-inspect clause)))))
    (_
     `((type . unknown)
       (value . ,cps)))))

(define (inspect-expression exp)
  "Inspect a CPS expression."
  (match exp
    (($ $const val)
     `((type . const)
       (value . ,val)))
    (($ $prim name)
     `((type . prim)
       (name . ,name)))
    (($ $call proc args)
     `((type . call)
       (procedure . ,proc)
       (arguments . ,args)))
    (($ $primcall name args)
     `((type . primcall)
       (name . ,name)
       (arguments . ,args)))
    (($ $values args)
     `((type . values)
       (arguments . ,args)))
    (_
     `((type . unknown)
       (value . ,exp)))))

(define (inspect-continuation k cps)
  "Inspect continuation K in CPS."
  (find-continuation k cps))

(define (list-continuations cps)
  "List all continuations in CPS."
  (let ((conts '()))
    (define (visit-cps term)
      (match term
        (($ $continue k _ exp)
         (set! conts (cons k conts))
         (visit-exp exp))
        (($ $kargs _ _ body)
         (visit-cps body))
        (($ $kfun _ _ _ _ clause)
         (when clause (visit-cps clause)))
        (_ #f)))
    
    (define (visit-exp exp)
      (match exp
        (($ $branch k exp)
         (set! conts (cons k conts))
         (visit-exp exp))
        (_ #f)))
    
    (visit-cps cps)
    (reverse conts)))

(define (find-continuation k cps)
  "Find continuation K in CPS term."
  (match cps
    (($ $continue k* _ _) 
     (and (eq? k k*) cps))
    (($ $kargs _ _ body)
     (find-continuation k body))
    (($ $kfun _ _ _ _ clause)
     (and clause (find-continuation k clause)))
    (_ #f)))

(define (inspect-term term)
  "General term inspection."
  (cps-inspect term))