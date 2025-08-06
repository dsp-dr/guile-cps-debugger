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

(define-module (cps-debugger compat)
  #:use-module (system base compile)
  #:use-module (language tree-il)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (compile-to-cps-compat
            cps-available?
            tree-il->pseudo-cps))

;;; Commentary:
;;;
;;; Compatibility layer for Guile 3.0+ CPS.
;;; This module requires Guile 3.0 or later.
;;;
;;; Code:

;; Ensure we're running on Guile 3.0+
(unless (and (defined? 'version) 
             (string>=? (version) "3.0.0"))
  (error "guile-cps-debugger requires Guile 3.0 or later"))

(define (cps-available?)
  "Check if CPS modules are available (should always be true in Guile 3.0+)."
  (false-if-exception
   (resolve-module '(language cps))))

(define (compile-to-cps-compat form)
  "Compile FORM to CPS (Guile 3.0+ required)."
  ;; Direct compilation to CPS - Guile 3.0+ only
  (compile form #:to 'cps))

(define (tree-il->pseudo-cps tree-il)
  "Convert Tree-IL to a pseudo-CPS representation (Guile 3.0+)."
  (match tree-il
    (($ <lambda> src meta body)
     `(lambda ,meta ,(tree-il->pseudo-cps body)))
    
    (($ <lambda-case> src req opt rest kw inits gensyms body alt)
     `(lambda-case (,req ,opt ,rest ,kw)
                   ,gensyms
                   ,(tree-il->pseudo-cps body)
                   ,@(if alt (list (tree-il->pseudo-cps alt)) '())))
    
    ;; Guile 3.0+ has native <primcall>
    (($ <primcall> src name args)
     `(primcall ,name ,@(map tree-il->pseudo-cps args)))
    
    (($ <call> src proc args)
     `(call ,(tree-il->pseudo-cps proc)
            ,@(map tree-il->pseudo-cps args)))
    
    (($ <conditional> src test then else)
     `(if ,(tree-il->pseudo-cps test)
          ,(tree-il->pseudo-cps then)
          ,(tree-il->pseudo-cps else)))
    
    (($ <const> src val)
     `(const ,val))
    
    (($ <lexical-ref> src name gensym)
     name)
    
    (($ <lexical-set> src name gensym exp)
     `(set! ,name ,(tree-il->pseudo-cps exp)))
    
    (($ <toplevel-ref> src name)
     `(toplevel-ref ,name))
    
    (($ <toplevel-set> src name exp)
     `(toplevel-set! ,name ,(tree-il->pseudo-cps exp)))
    
    (($ <module-ref> src mod name public?)
     `(@ ,mod ,name))
    
    (($ <module-set> src mod name public? exp)
     `(set! (@ ,mod ,name) ,(tree-il->pseudo-cps exp)))
    
    (($ <toplevel-define> src name exp)
     `(define ,name ,(tree-il->pseudo-cps exp)))
    
    (($ <seq> src head tail)
     ;; Guile 3.0+ uses <seq> for sequences
     `(begin ,(tree-il->pseudo-cps head)
             ,(tree-il->pseudo-cps tail)))
    
    (($ <let> src names gensyms vals body)
     `(let ,(map list names (map tree-il->pseudo-cps vals))
        ,(tree-il->pseudo-cps body)))
    
    (($ <letrec> src in-order? names gensyms vals body)
     `(letrec ,(map list names (map tree-il->pseudo-cps vals))
        ,(tree-il->pseudo-cps body)))
    
    (($ <let-values> src exp body)
     `(let-values ,(tree-il->pseudo-cps exp)
        ,(tree-il->pseudo-cps body)))
    
    (_ tree-il)))