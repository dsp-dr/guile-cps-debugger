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
  #:re-export (cps-debug
               cps-step
               cps-inspect
               cps-pretty-print
               make-cps-debugger
               debugger-step
               debugger-continue
               debugger-break))

;;; Commentary:
;;;
;;; Main entry point for the CPS debugger.  This module re-exports
;;; the primary functionality from the various submodules.
;;;
;;; Code: