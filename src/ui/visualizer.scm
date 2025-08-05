;;; visualizer.scm --- CPS chain visualization
;;; Commentary:
;;;
;;; This module provides visualization capabilities for continuation chains,
;;; including ASCII tree views and Graphviz DOT format generation for
;;; visual analysis of CPS execution patterns.
;;;
;;; Code:

(define-module (cps-debugger ui visualizer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (cps-debugger analysis continuation)
  #:export (visualize-continuation-chain
            generate-dot-graph
            ascii-tree-view
            format-node
            colorize-output))

;;; Color codes for terminal output
(define *color-codes*
  '((reset . "\x1b[0m")
    (bold . "\x1b[1m")
    (red . "\x1b[31m")
    (green . "\x1b[32m")
    (yellow . "\x1b[33m")
    (blue . "\x1b[34m")
    (magenta . "\x1b[35m")
    (cyan . "\x1b[36m")))

;;; Main visualization function
(define (visualize-continuation-chain chain-info format)
  "Visualize continuation chain in specified format."
  (case format
    ((ascii) (ascii-tree-view chain-info))
    ((dot graphviz) (generate-dot-graph chain-info))
    ((sexp) (pretty-print chain-info))
    (else (error "Unknown visualization format" format))))

;;; ASCII tree view
(define (ascii-tree-view chain-info)
  "Generate ASCII tree representation of continuation chain."
  (let ((closures (continuation-closures chain-info))
        (metadata (continuation-metadata chain-info))
        (use-color (and (isatty? (current-output-port))
                       (getenv "TERM")
                       (not (string=? (getenv "TERM") "dumb")))))
    
    (string-append
     (if use-color
         (colorize 'bold "Continuation Chain Tree:\n")
         "Continuation Chain Tree:\n")
     (format-tree-header metadata use-color)
     "\n"
     (format-closure-tree closures use-color)
     "\n"
     (format-tree-footer chain-info use-color))))

(define (format-tree-header metadata use-color)
  "Format tree header with metadata."
  (let ((length (assq-ref metadata 'length))
        (unique (assq-ref metadata 'unique-procedures)))
    (format #f "~a~a continuations, ~a unique procedures~a"
            (if use-color (colorize 'cyan "") "")
            length
            unique
            (if use-color (colorize 'reset "") ""))))

(define (format-closure-tree closures use-color)
  "Format closures as tree structure."
  (define (format-closure closure index depth)
    (let* ((proc (assq-ref closure 'procedure))
           (arity (assq-ref closure 'arity))
           (has-cont (assq-ref closure 'has-continuation))
           (indent (make-string (* depth 2) #\space))
           (connector (if (= depth 0) "" "├─ "))
           (proc-name (or (and (procedure? proc) 
                              (procedure-name proc))
                          (format #f "λ~a" index))))
      (format #f "~a~a~a~a [arity: ~a]~a~a"
              indent
              connector
              (if (and use-color has-cont) (colorize 'green "") "")
              proc-name
              arity
              (if has-cont " ◆" "")
              (if use-color (colorize 'reset "") ""))))
  
  (string-join
   (map (lambda (closure index)
          (format-closure closure index 
                         (calculate-depth closure index closures)))
        closures
        (iota (length closures)))
   "\n"))

(define (calculate-depth closure index closures)
  "Calculate visual depth for closure."
  ;; Simple heuristic: continuations increase depth
  (let loop ((i 0)
             (depth 0))
    (if (>= i index)
        depth
        (if (assq-ref (list-ref closures i) 'has-continuation)
            (loop (+ i 1) (+ depth 1))
            (loop (+ i 1) depth)))))

(define (format-tree-footer chain-info use-color)
  "Format tree footer with statistics."
  (let* ((memory (continuation-memory chain-info))
         (total-mem (assq-ref memory 'total-estimated))
         (depth (continuation-depth chain-info)))
    (format #f "~aMax depth: ~a, Est. memory: ~:d bytes~a"
            (if use-color (colorize 'yellow "") "")
            depth
            total-mem
            (if use-color (colorize 'reset "") ""))))

;;; Graphviz DOT generation
(define (generate-dot-graph chain-info)
  "Generate Graphviz DOT format for continuation chain."
  (let ((closures (continuation-closures chain-info))
        (metadata (continuation-metadata chain-info)))
    (string-append
     "digraph CPS {\n"
     "  rankdir=TB;\n"
     "  node [shape=box, style=rounded, fontname=\"Arial\"];\n"
     "  edge [fontname=\"Arial\"];\n"
     "\n"
     (format-graph-header metadata)
     "\n"
     (format-nodes closures)
     "\n"
     (format-edges closures)
     "}\n")))

(define (format-graph-header metadata)
  "Format DOT graph header."
  (format #f "  // CPS Chain: ~a continuations, ~a unique procedures\n"
          (assq-ref metadata 'length)
          (assq-ref metadata 'unique-procedures)))

(define (format-nodes closures)
  "Format nodes for DOT graph."
  (string-join
   (map (lambda (closure index)
          (format-dot-node closure index))
        closures
        (iota (length closures)))
   "\n"))

(define (format-dot-node closure index)
  "Format single node for DOT graph."
  (let* ((proc (assq-ref closure 'procedure))
         (arity (assq-ref closure 'arity))
         (has-cont (assq-ref closure 'has-continuation))
         (proc-name (or (and (procedure? proc)
                            (procedure-name proc))
                       (format #f "λ~a" index)))
         (color (if has-cont "lightgreen" "lightblue")))
    (format #f "  n~a [label=\"~a\\narity: ~a\", fillcolor=~a, style=filled];"
            index proc-name arity color)))

(define (format-edges closures)
  "Format edges for DOT graph."
  (let loop ((closures closures)
             (index 0)
             (edges '()))
    (if (null? closures)
        (string-join (reverse edges) "\n")
        (let ((closure (car closures))
              (next-index (+ index 1)))
          (if (and (assq-ref closure 'has-continuation)
                   (< next-index (length closures)))
              (loop (cdr closures)
                    next-index
                    (cons (format #f "  n~a -> n~a [label=\"cont\"];"
                                  index next-index)
                          edges))
              (loop (cdr closures) next-index edges))))))

;;; Node formatting
(define (format-node proc args depth)
  "Format a single node for display."
  (let* ((indent (make-string (* depth 2) #\space))
         (proc-str (format-procedure proc))
         (args-str (format-arguments args)))
    (format #f "~a~a(~a)" indent proc-str args-str)))

(define (format-procedure proc)
  "Format procedure for display."
  (cond
   ((procedure? proc)
    (or (procedure-name proc) "<anonymous>"))
   ((symbol? proc)
    (symbol->string proc))
   (else
    (format #f "~s" proc))))

(define (format-arguments args)
  "Format arguments for display."
  (string-join
   (map (lambda (arg)
          (cond
           ((procedure? arg) "<continuation>")
           ((list? arg) (format #f "(~a...)" (length arg)))
           ((string? arg) (format #f "~s" (substring arg 0 (min 10 (string-length arg)))))
           (else (format #f "~s" arg))))
        args)
   ", "))

;;; Colorization
(define (colorize color text)
  "Colorize text for terminal output."
  (let ((code (assq-ref *color-codes* color)))
    (if code
        (string-append code text (assq-ref *color-codes* 'reset))
        text)))

(define (colorize-output text color-map)
  "Apply color map to text output."
  (let loop ((patterns (map (lambda (entry)
                             (cons (car entry) 
                                   (assq-ref *color-codes* (cdr entry))))
                           color-map))
             (result text))
    (if (null? patterns)
        result
        (let* ((pattern (car patterns))
               (regex (car pattern))
               (color (cdr pattern)))
          ;; Note: Simple implementation without regex
          ;; Real implementation would use regex matching
          (loop (cdr patterns) result)))))

;;; Utility functions
(define (isatty? port)
  "Check if PORT is a terminal."
  ;; Simplified implementation
  #t)

(define (procedure-name proc)
  "Get procedure name if available."
  ;; This would use Guile's procedure properties in a real implementation
  #f)