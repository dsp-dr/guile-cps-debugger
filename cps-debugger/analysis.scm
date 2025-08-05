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

(define-module (cps-debugger analysis)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (ice-9 match)
  #:export (analyze-cps
            count-continuations
            find-call-sites
            analyze-variables
            compute-free-variables))

;;; Commentary:
;;;
;;; Static analysis tools for CPS terms.
;;;
;;; Code:

(define (analyze-cps cps)
  "Perform a comprehensive analysis of CPS term."
  `((continuation-count . ,(count-continuations cps))
    (call-sites . ,(find-call-sites cps))
    (variable-info . ,(analyze-variables cps))
    (free-variables . ,(compute-free-variables cps))))

(define (count-continuations cps)
  "Count the number of continuations in CPS."
  (let ((count 0))
    (define (visit term)
      (match term
        (($ $continue k _ exp)
         (set! count (+ count 1))
         (visit-exp exp))
        (($ $kargs _ _ body)
         (set! count (+ count 1))
         (visit body))
        (($ $kreceive _ _)
         (set! count (+ count 1)))
        (($ $kfun _ _ _ _ clause)
         (set! count (+ count 1))
         (when clause (visit clause)))
        (_ #f)))
    
    (define (visit-exp exp)
      (match exp
        (($ $branch _ exp)
         (visit-exp exp))
        (_ #f)))
    
    (visit cps)
    count))

(define (find-call-sites cps)
  "Find all call sites in CPS."
  (let ((sites '()))
    (define (visit term)
      (match term
        (($ $continue k src ($ $call proc args))
         (set! sites (cons `((continuation . ,k)
                            (source . ,src)
                            (procedure . ,proc)
                            (arguments . ,args))
                          sites)))
        (($ $continue k src ($ $primcall name args))
         (set! sites (cons `((continuation . ,k)
                            (source . ,src)
                            (primitive . ,name)
                            (arguments . ,args))
                          sites)))
        (($ $continue _ _ exp)
         (visit-exp exp))
        (($ $kargs _ _ body)
         (visit body))
        (($ $kfun _ _ _ _ clause)
         (when clause (visit clause)))
        (_ #f)))
    
    (define (visit-exp exp)
      (match exp
        (($ $branch _ exp)
         (visit-exp exp))
        (_ #f)))
    
    (visit cps)
    (reverse sites)))

(define (analyze-variables cps)
  "Analyze variable usage in CPS."
  (let ((defined '())
        (used '()))
    
    (define (visit term)
      (match term
        (($ $kargs names syms body)
         (set! defined (append syms defined))
         (visit body))
        (($ $continue _ _ exp)
         (visit-exp exp))
        (($ $kfun _ _ self _ clause)
         (when self
           (set! defined (cons self defined)))
         (when clause (visit clause)))
        (_ #f)))
    
    (define (visit-exp exp)
      (match exp
        (($ $call proc args)
         (set! used (cons proc used))
         (set! used (append args used)))
        (($ $primcall _ args)
         (set! used (append args used)))
        (($ $values args)
         (set! used (append args used)))
        (($ $branch _ exp)
         (visit-exp exp))
        (_ #f)))
    
    (visit cps)
    `((defined . ,(delete-duplicates defined))
      (used . ,(delete-duplicates used)))))

(define (compute-free-variables cps)
  "Compute free variables in CPS term."
  (let ((info (analyze-variables cps)))
    (lset-difference eq? 
                     (assq-ref info 'used)
                     (assq-ref info 'defined))))