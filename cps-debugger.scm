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

(define-module (cps-debugger)
  #:use-module (cps-debugger core)
  #:use-module (cps-debugger inspector)
  #:use-module (cps-debugger pretty)
  #:use-module (cps-debugger compat)
  #:use-module (cps-debugger stepper)
  #:use-module (cps-debugger session)
  #:use-module (cps-debugger cli)
  #:use-module (cps-debugger analysis)
  #:re-export (;; Core
               cps-debug
               cps-step
               make-cps-debugger
               debugger-step
               debugger-continue
               debugger-break
               
               ;; Inspection
               cps-inspect
               cps-pretty-print
               
               ;; Analysis
               analyze-cps
               count-nodes
               
               ;; Stepper
               make-stepper
               step-forward
               step-backward
               step-into
               step-over
               step-out
               add-breakpoint
               remove-breakpoint
               step-to-breakpoint
               
               ;; Session
               make-debug-session
               add-watch
               remove-watch
               save-session
               load-session
               
               ;; CLI
               start-debugger-cli
               debugger-repl))

;;; Commentary:
;;;
;;; Main entry point for the CPS debugger.  This module re-exports
;;; the primary functionality from the various submodules.
;;;
;;; Code: