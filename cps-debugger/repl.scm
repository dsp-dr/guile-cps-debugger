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

(define-module (cps-debugger repl)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (cps-debugger core)
  #:use-module (cps-debugger pretty)
  #:use-module (cps-debugger inspector)
  #:export (install-cps-commands!))

;;; Commentary:
;;;
;;; REPL integration for the CPS debugger.
;;;
;;; Code:

(define (install-cps-commands!)
  "Install CPS debugging commands in the REPL."
  
  (define-meta-command ((cps-debug guile) repl form)
    "cps-debug FORM
Debug the CPS compilation of FORM."
    (let ((debugger (cps-debug form)))
      (format #t "CPS debugger started.~%")
      (format #t "Use ,cps-step to step through, ,cps-inspect to inspect.~%")))
  
  (define-meta-command ((cps-step guile) repl form)
    "cps-step FORM
Step through CPS transformation of FORM."
    (cps-step form))
  
  (define-meta-command ((cps-inspect guile) repl form)
    "cps-inspect FORM
Inspect the CPS representation of FORM."
    (let* ((cps (cps-step form))
           (info (cps-inspect cps)))
      (format #t "~%CPS inspection:~%")
      (for-each (lambda (pair)
                  (format #t "  ~a: ~s~%" (car pair) (cdr pair)))
                info)))
  
  (define-meta-command ((cps-pretty guile) repl form)
    "cps-pretty FORM
Pretty-print the CPS representation of FORM."
    (let ((cps (cps-step form)))
      (format #t "~%Pretty-printed CPS:~%")
      (cps-pretty-print cps)))
  
  'installed)

;; Auto-install commands when module is loaded
(install-cps-commands!)