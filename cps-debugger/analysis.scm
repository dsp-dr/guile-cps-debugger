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
  #:use-module (ice-9 receive)
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
  "Analyze variable usage in pseudo-CPS using functional approach."
  (define (collect-variables term defined used in-binding?)
    "Functionally collect defined and used variables."
    (match term
      ;; Symbol - either defined or used based on context
      ((? symbol? var)
       (if in-binding?
           (values (cons var defined) used)
           (values defined (cons var used))))
      
      ;; Lambda case - defines parameters
      (('lambda-case (req opt rest kw) gensyms body . alt)
       (let* ((new-defined (append req 
                                  (or opt '())
                                  (if rest (list rest) '())
                                  defined))
              (body-result (collect-variables body new-defined used #f)))
         (if (pair? alt)
             (collect-variables (car alt) 
                               (car body-result) 
                               (cdr body-result) 
                               #f)
             body-result)))
      
      ;; Let bindings - defines variables and evaluates values
      (('let bindings body)
       (let loop ((bindings bindings)
                  (def defined)
                  (use used))
         (if (null? bindings)
             (collect-variables body 
                               (append (map car bindings) def)
                               use 
                               #f)
             (let ((binding (car bindings)))
               (receive (new-def new-use)
                        (collect-variables (cadr binding) def use #f)
                 (loop (cdr bindings) new-def new-use))))))
      
      ;; Call expressions - all arguments are used
      (('call proc . args)
       (fold (lambda (arg acc)
               (receive (def use) 
                        (collect-variables arg (car acc) (cdr acc) #f)
                 (cons def use)))
             (collect-variables proc defined used #f)
             args))
      
      ;; Primitive calls - all arguments are used
      (('primcall name . args)
       (fold (lambda (arg acc)
               (receive (def use)
                        (collect-variables arg (car acc) (cdr acc) #f)
                 (cons def use)))
             (cons defined used)
             args))
      
      ;; Conditionals - test, then, and else are all used
      (('if test then else)
       (receive (def1 use1) (collect-variables test defined used #f)
         (receive (def2 use2) (collect-variables then def1 use1 #f)
           (collect-variables else def2 use2 #f))))
      
      ;; Default case
      (_ (values defined used))))
  
  (receive (defined used) 
           (collect-variables cps '() '() #f)
    `((defined . ,(delete-duplicates defined))
      (used . ,(delete-duplicates used)))))

(define (compute-free-variables cps)
  "Compute free variables in CPS term."
  (let ((info (analyze-variables cps)))
    (lset-difference eq? 
                     (assq-ref info 'used)
                     (assq-ref info 'defined))))