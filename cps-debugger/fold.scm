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

(define-module (cps-debugger fold)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)  ; cut for partial application
  #:export (cps-fold
            cps-fold-with-path
            cps-map
            cps-filter
            cps-find
            cps-count
            cps-collect))

;;; Commentary:
;;;
;;; Generic folding operations for CPS and pseudo-CPS terms.
;;; Provides functional traversal patterns to replace imperative loops.
;;;
;;; Code:

(define* (cps-fold proc init term #:key (depth-first? #t))
  "Fold PROC over CPS TERM with INIT value.
PROC is called as (proc term accumulator).
If DEPTH-FIRST? is true (default), process children before parent."
  (define (fold-term term acc)
    (let ((acc (if depth-first? acc (proc term acc))))
      (let ((acc (cond
                  ;; Handle pseudo-CPS structures
                  ((and (pair? term) (symbol? (car term)))
                   (match term
                     (('lambda meta body)
                      (fold-term body acc))
                     (('lambda-case args gensyms body . alt)
                      (let ((acc (fold-term body acc)))
                        (if (pair? alt)
                            (fold-term (car alt) acc)
                            acc)))
                     (('call proc . args)
                      (fold fold-term (fold-term proc acc) args))
                     (('primcall name . args)
                      (fold fold-term acc args))
                     (('if test then else)
                      (fold-term else
                                (fold-term then
                                          (fold-term test acc))))
                     (('let bindings body)
                      (let ((acc (fold (lambda (binding acc)
                                        (fold-term (cadr binding) acc))
                                      acc bindings)))
                        (fold-term body acc)))
                     (('begin . exps)
                      (fold fold-term acc exps))
                     (_ acc)))
                  ;; Handle lists recursively
                  ((list? term)
                   (fold fold-term acc term))
                  ;; Base case
                  (else acc))))
        (if depth-first? (proc term acc) acc))))
  
  (fold-term term init))

(define* (cps-fold-with-path proc init term #:key (depth-first? #t))
  "Like cps-fold but PROC receives (term path accumulator).
PATH is a list of indices showing location in tree."
  (define (fold-term term path acc)
    (let ((acc (if depth-first? acc (proc term path acc))))
      (let ((acc (cond
                  ((and (pair? term) (symbol? (car term)))
                   (match term
                     (('lambda meta body)
                      (fold-term body (cons 0 path) acc))
                     (('lambda-case args gensyms body . alt)
                      (let ((acc (fold-term body (cons 0 path) acc)))
                        (if (pair? alt)
                            (fold-term (car alt) (cons 1 path) acc)
                            acc)))
                     (('call proc . args)
                      (let ((acc (fold-term proc (cons 0 path) acc)))
                        (fold (lambda (arg idx acc)
                                (fold-term arg (cons idx path) acc))
                              acc args (iota (length args) 1))))
                     (('if test then else)
                      (fold-term else (cons 2 path)
                                (fold-term then (cons 1 path)
                                          (fold-term test (cons 0 path) acc))))
                     (_ acc)))
                  ((list? term)
                   (fold (lambda (item idx acc)
                          (fold-term item (cons idx path) acc))
                        acc term (iota (length term))))
                  (else acc))))
        (if depth-first? (proc term path acc) acc))))
  
  (fold-term term '() init))

(define (cps-map proc term)
  "Map PROC over all nodes in CPS TERM."
  (reverse (cps-fold (lambda (t acc) (cons (proc t) acc)) '() term)))

(define (cps-filter pred term)
  "Filter nodes in CPS TERM that satisfy PRED."
  (reverse (cps-fold (lambda (t acc)
                      (if (pred t)
                          (cons t acc)
                          acc))
                    '() term)))

(define (cps-find pred term)
  "Find first node in CPS TERM that satisfies PRED."
  (call/cc
   (lambda (return)
     (cps-fold (lambda (t acc)
                (when (pred t)
                  (return t))
                acc)
              #f term
              #:depth-first? #f))))

(define (cps-count pred term)
  "Count nodes in CPS TERM that satisfy PRED."
  (cps-fold (lambda (t acc)
             (if (pred t) (+ acc 1) acc))
           0 term))

(define (cps-collect type term)
  "Collect all nodes of TYPE from CPS TERM."
  (cps-filter (lambda (node)
               (and (pair? node)
                    (eq? (car node) type)))
             term))