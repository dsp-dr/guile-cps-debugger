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
  #:use-module (ice-9 match)
  #:use-module (cps-debugger compat)
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
  "Inspect a CPS or pseudo-CPS term, returning information about its structure."
  (cond
   ;; Handle pseudo-CPS from tree-il->pseudo-cps
   ((and (pair? cps) (symbol? (car cps)))
    (match cps
      (('lambda meta body)
       `((type . lambda)
         (meta . ,meta)
         (body . ,(cps-inspect body))))
      (('lambda-case args gensyms body . alt)
       `((type . lambda-case)
         (args . ,args)
         (gensyms . ,gensyms)
         (body . ,(cps-inspect body))
         (alt . ,(and (pair? alt) (cps-inspect (car alt))))))
      (('call proc . args)
       `((type . call)
         (procedure . ,(cps-inspect proc))
         (arguments . ,(map cps-inspect args))))
      (('primcall name . args)
       `((type . primcall)
         (name . ,name)
         (arguments . ,(map cps-inspect args))))
      (('if test then else)
       `((type . conditional)
         (test . ,(cps-inspect test))
         (then . ,(cps-inspect then))
         (else . ,(cps-inspect else))))
      (('const val)
       `((type . const)
         (value . ,val)))
      (('let bindings body)
       `((type . let)
         (bindings . ,bindings)
         (body . ,(cps-inspect body))))
      (('begin . exps)
       `((type . sequence)
         (expressions . ,(map cps-inspect exps))))
      (_
       `((type . expression)
         (value . ,cps)))))
   ;; Handle symbols (variable references)
   ((symbol? cps)
    `((type . variable)
      (name . ,cps)))
   ;; Fallback for other values
   (else
    `((type . unknown)
      (value . ,cps)))))

(define (inspect-expression exp)
  "Inspect a CPS expression (compatibility stub)."
  (cps-inspect exp))

(define (inspect-continuation k cps)
  "Inspect continuation K in CPS (compatibility stub)."
  `((type . continuation)
    (name . ,k)
    (in . ,cps)))

(define (list-continuations cps)
  "List all continuations in CPS (pseudo-implementation)."
  ;; For pseudo-CPS, we don't have real continuations
  ;; Return empty list for compatibility
  '())

(define (find-continuation k cps)
  "Find continuation K in CPS term (compatibility stub)."
  #f)

(define (inspect-term term)
  "General term inspection."
  (cps-inspect term))