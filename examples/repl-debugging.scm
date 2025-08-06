#!/usr/bin/env guile
!#

;;; Example: REPL-based debugging

(add-to-load-path "..")
(use-modules (cps-debugger)
             (cps-debugger repl-commands)
             (system repl repl))

(display "=== REPL Debugging Example ===\n\n")

(display "This example demonstrates the enhanced REPL commands.\n")
(display "The following commands are available:\n\n")

(display "Basic commands:\n")
(display "  ,cps-debug FORM    - Start debugging FORM\n")
(display "  ,cps-step FORM     - Step through FORM\n")
(display "  ,cps-inspect       - Inspect current position\n")
(display "  ,cps-pretty        - Pretty-print current position\n")
(display "  ,cps-analyze       - Analyze current position\n")
(display "\n")

(display "Navigation commands:\n")
(display "  ,step              - Step forward\n")
(display "  ,step-back         - Step backward\n")
(display "  ,step-into         - Step into expression\n")
(display "  ,step-over         - Step over expression\n")
(display "  ,step-out          - Step out of context\n")
(display "\n")

(display "Breakpoint commands:\n")
(display "  ,break POS         - Add breakpoint\n")
(display "  ,unbreak POS       - Remove breakpoint\n")
(display "  ,breakpoints       - List breakpoints\n")
(display "  ,continue          - Continue to breakpoint\n")
(display "\n")

(display "State commands:\n")
(display "  ,cps-state         - Show debugger state\n")
(display "  ,cps-reset         - Reset debugger\n")
(display "\n")

(display "Example session:\n")
(display "---------------\n\n")

;; Demonstrate a debugging session
(let* ((form '(lambda (x) 
                (let ((y (* x 2)))
                  (if (> y 10)
                      y
                      (+ y 5)))))
       (debugger (cps-debug form)))
  
  (display "Started debugging: ")
  (display form)
  (newline)
  (newline)
  
  ;; Set the current stepper for REPL commands
  (let ((stepper (make-stepper (debugger-term debugger))))
    (set-current-stepper! stepper)
    
    (display "Current state:\n")
    (stepper-print-state stepper)
    
    (display "\nTo continue interactively, start a REPL:\n")
    (display "  scheme@(guile-user)> ,cps-debug (lambda (x) (+ x 1))\n")
    (display "  scheme@(guile-user)> ,step\n")
    (display "  scheme@(guile-user)> ,cps-inspect\n")
    (display "  scheme@(guile-user)> ,break (0 1)\n")
    (display "  scheme@(guile-user)> ,continue\n")))

(display "\n")
(display "=" 65)
(display "\n\n")

(display "Starting REPL with CPS debugging commands...\n")
(display "Try the commands above. Type ,q to quit.\n\n")

;; Start the REPL
(start-repl)