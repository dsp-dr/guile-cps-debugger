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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)  ; cut
  #:use-module (cps-debugger compat)
  #:use-module (cps-debugger fold)
  #:export (analyze-cps
            count-nodes
            find-call-sites
            analyze-variables
            compute-free-variables))

;;; Commentary:
;;;
;;; Static analysis tools for CPS terms.
;;;
;;; Code:

(define (analyze-cps cps)
  "Perform a comprehensive analysis of CPS or pseudo-CPS term."
  `((node-count . ,(count-nodes cps))
    (call-sites . ,(find-call-sites cps))
    (variable-info . ,(analyze-variables cps))
    (free-variables . ,(compute-free-variables cps))))

(define (count-nodes cps)
  "Count the number of nodes in CPS or pseudo-CPS."
  (cps-count (const #t) cps))

(define (find-call-sites cps)
  "Find all call sites in pseudo-CPS."
  (cps-fold (lambda (term acc)
             (match term
               (('call proc . args)
                (cons `((type . call)
                        (procedure . ,proc)
                        (arguments . ,args))
                      acc))
               (('primcall name . args)
                (cons `((type . primcall)
                        (primitive . ,name)
                        (arguments . ,args))
                      acc))
               (_ acc)))
           '() cps))

(define (analyze-variables cps)
  "Analyze variable usage in pseudo-CPS."
  (let ((defined '())
        (used '()))
    
    (define (visit-def term)
      (cond
       ((and (pair? term) (symbol? (car term)))
        (match term
          (('lambda-case (req opt rest kw) gensyms body . alt)
           (set! defined (append req defined))
           (when opt (set! defined (append opt defined)))
           (when rest (set! defined (cons rest defined)))
           (visit-def body)
           (when (pair? alt) (visit-def (car alt))))
          (('let bindings body)
           (set! defined (append (map car bindings) defined))
           (for-each (lambda (b) (visit-use (cadr b))) bindings)
           (visit-def body))
          (_ (visit-use term))))
       (else (visit-use term))))
    
    (define (visit-use term)
      (cond
       ((symbol? term)
        (set! used (cons term used)))
       ((and (pair? term) (symbol? (car term)))
        (match term
          (('call proc . args)
           (visit-use proc)
           (for-each visit-use args))
          (('primcall name . args)
           (for-each visit-use args))
          (('if test then else)
           (visit-use test)
           (visit-use then)
           (visit-use else))
          (('lambda meta body)
           (visit-def body))
          (('lambda-case args gensyms body . alt)
           (visit-def body)
           (when (pair? alt) (visit-def (car alt))))
          (('let bindings body)
           (visit-def term))
          (('begin . exps)
           (for-each visit-use exps))
          (_ #f)))
       (else #f)))
    
    (visit-def cps)
    `((defined . ,(delete-duplicates defined))
      (used . ,(delete-duplicates used)))))

(define (compute-free-variables cps)
  "Compute free variables in CPS term."
  (let ((info (analyze-variables cps)))
    (lset-difference eq? 
                     (assq-ref info 'used)
                     (assq-ref info 'defined))))