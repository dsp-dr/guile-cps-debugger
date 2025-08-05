#!/bin/sh

# Automated demo for recording
# This creates a clean, scripted demo

echo "=== Guile CPS Debugger Demo ==="
echo
sleep 2

echo "The CPS Debugger provides tools for debugging Guile's intermediate representation."
echo
sleep 2

echo "Let's see how Scheme code is compiled to Tree-IL (precursor to CPS):"
echo
sleep 2

echo "Example 1: Simple function"
echo "  (lambda (x) (+ x 1))"
echo
guile -c "(use-modules (system base compile) (ice-9 pretty-print)) (pretty-print (compile '(lambda (x) (+ x 1)) #:to 'tree-il))"
echo
sleep 3

echo "Example 2: Conditional expression"
echo "  (lambda (x) (if (> x 0) x (- x)))"
echo
guile -c "(use-modules (system base compile) (ice-9 pretty-print)) (pretty-print (compile '(lambda (x) (if (> x 0) x (- x))) #:to 'tree-il))"
echo
sleep 3

echo "Project structure:"
echo
tree -I '*.go' --dirsfirst -L 2
echo
sleep 2

echo "The debugger includes:"
echo "  • Core debugging engine"
echo "  • CPS term inspector"
echo "  • Pretty-printing utilities"
echo "  • REPL integration"
echo "  • Static analysis tools"
echo
sleep 3

echo "For full CPS debugging, use Guile 3.0+ with:"
echo "  (use-modules (cps-debugger))"
echo "  ,cps-pretty (lambda (x) (+ x 1))"
echo
sleep 2

echo "Demo complete! See README.org for more information."