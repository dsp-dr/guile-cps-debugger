;;; visualizer.scm - Visualization for Guile Scheme Evaluation Model
;;; 
;;; Provides tools to visualize:
;;; - Stack frames and continuations
;;; - CPS transformation graphs
;;; - Evaluation traces as diagrams
;;; - Export to Mermaid, DOT, and SVG formats

(define-module (cps-debugger visualizer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system vm debug)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:use-module (system vm trace)
  #:export (stack->mermaid
            trace->mermaid
            continuation-chain->mermaid
            eval-model->diagram
            visualize-cps-flow
            capture-evaluation-trace
            frame->node
            create-call-graph))

;;;; Stack Visualization

(define (stack->mermaid . args)
  "Convert current stack to Mermaid diagram format"
  (let ((stack (if (null? args)
                   (make-stack #t)
                   (car args))))
    (string-append
     "graph TD\n"
     "    classDef frameNode fill:#f9f,stroke:#333,stroke-width:2px\n"
     "    classDef currentNode fill:#9f9,stroke:#333,stroke-width:4px\n"
     (stack-frames->mermaid stack))))

(define (stack-frames->mermaid stack)
  "Convert stack frames to Mermaid nodes and edges"
  (let ((frames (stack-frames stack)))
    (string-join
     (map-indexed
      (lambda (frame idx)
        (let* ((next-idx (+ idx 1))
               (has-next? (< next-idx (length frames)))
               (node-def (frame->mermaid-node frame idx))
               (edge-def (if has-next?
                            (format #f "    F~a --> F~a\n" idx next-idx)
                            "")))
          (string-append node-def edge-def)))
      frames)
     "")))

(define (frame->mermaid-node frame idx)
  "Convert a single frame to Mermaid node"
  (let* ((proc-name (frame-procedure-name frame))
         (args (frame-arguments frame))
         (label (format #f "~a~a" 
                       (or proc-name "<anonymous>")
                       (if (null? args)
                           ""
                           (format #f "<br/>args: ~s" args)))))
    (format #f "    F~a[\"~a\"]:::frameNode\n" idx label)))

(define (stack-frames stack)
  "Extract all frames from a stack"
  (let loop ((i 0) (frames '()))
    (if (>= i (stack-length stack))
        (reverse frames)
        (loop (+ i 1) 
              (cons (stack-ref stack i) frames)))))

(define (map-indexed proc lst)
  "Map with index"
  (let loop ((lst lst) (idx 0) (result '()))
    (if (null? lst)
        (reverse result)
        (loop (cdr lst)
              (+ idx 1)
              (cons (proc (car lst) idx) result)))))

;;;; Trace Visualization

(define (trace->mermaid trace-output)
  "Convert trace output to Mermaid sequence diagram"
  (string-append
   "sequenceDiagram\n"
   "    participant User\n"
   "    participant Eval\n"
   "    participant CPS\n"
   (trace-lines->mermaid (parse-trace trace-output))))

(define (parse-trace trace-str)
  "Parse trace output into structured data"
  ;; Simple parser for trace output
  (let ((lines (string-split trace-str #\newline)))
    (filter-map
     (lambda (line)
       (cond
        ((string-contains line "call:")
         `(call ,(extract-proc-name line)))
        ((string-contains line "return:")
         `(return ,(extract-value line)))
        (else #f)))
     lines)))

(define (extract-proc-name line)
  "Extract procedure name from trace line"
  (let ((match (string-match "\\(([^ ]+)" line)))
    (if match
        (match:substring match 1)
        "<unknown>")))

(define (extract-value line)
  "Extract return value from trace line"
  (let ((match (string-match "=> (.+)$" line)))
    (if match
        (match:substring match 1)
        "<value>")))

(define (trace-lines->mermaid trace-data)
  "Convert parsed trace to Mermaid sequence"
  (string-join
   (map (lambda (item)
          (match item
            (('call proc)
             (format #f "    User->>Eval: ~a\n    Eval->>CPS: transform" proc))
            (('return val)
             (format #f "    CPS-->>Eval: ~a\n    Eval-->>User: result" val))
            (_ "")))
        trace-data)
   ""))

;;;; Continuation Chain Visualization

(define (continuation-chain->mermaid cont-list)
  "Visualize continuation chain as flowchart"
  (string-append
   "graph LR\n"
   "    classDef contNode fill:#ff9,stroke:#333,stroke-width:2px\n"
   "    classDef valueNode fill:#9ff,stroke:#333,stroke-width:2px\n"
   (continuations->nodes cont-list)
   (continuations->edges cont-list)))

(define (continuations->nodes cont-list)
  "Generate nodes for continuations"
  (string-join
   (map-indexed
    (lambda (cont idx)
      (format #f "    K~a[\"λ(~a)<br/>~a\"]:::contNode\n"
              idx
              (continuation-params cont)
              (continuation-body-summary cont)))
    cont-list)
   ""))

(define (continuation-params cont)
  "Extract parameter names from continuation"
  ;; Simplified - would need proper analysis
  "result")

(define (continuation-body-summary cont)
  "Create summary of continuation body"
  ;; Simplified - would need proper analysis
  "...")

(define (continuations->edges cont-list)
  "Generate edges between continuations"
  (string-join
   (map-indexed
    (lambda (cont idx)
      (if (< idx (- (length cont-list) 1))
          (format #f "    K~a --> K~a\n" idx (+ idx 1))
          ""))
    cont-list)
   ""))

;;;; Evaluation Model Diagram

(define (eval-model->diagram expr)
  "Create complete evaluation model diagram for expression"
  (string-append
   "graph TB\n"
   "    subgraph \"Input\"\n"
   "        INPUT[\"" (format #f "~s" expr) "\"]\n"
   "    end\n"
   "    \n"
   "    subgraph \"CPS Transform\"\n"
   (cps-transform-diagram expr)
   "    end\n"
   "    \n"
   "    subgraph \"Evaluation\"\n"
   (evaluation-diagram expr)
   "    end\n"
   "    \n"
   "    subgraph \"Result\"\n"
   "        RESULT[\"output\"]\n"
   "    end\n"
   "    \n"
   "    INPUT --> CPS1\n"
   "    CPS3 --> EVAL1\n"
   "    EVAL3 --> RESULT\n"))

(define (cps-transform-diagram expr)
  "Generate CPS transformation steps"
  (string-append
   "        CPS1[\"Parse AST\"]\n"
   "        CPS2[\"α-conversion\"]\n"
   "        CPS3[\"CPS transform\"]\n"
   "        CPS1 --> CPS2\n"
   "        CPS2 --> CPS3\n"))

(define (evaluation-diagram expr)
  "Generate evaluation steps"
  (string-append
   "        EVAL1[\"Create continuations\"]\n"
   "        EVAL2[\"Apply functions\"]\n"
   "        EVAL3[\"Collect results\"]\n"
   "        EVAL1 --> EVAL2\n"
   "        EVAL2 --> EVAL3\n"))

;;;; CPS Flow Visualization

(define (visualize-cps-flow proc args)
  "Visualize CPS execution flow"
  (let* ((trace-data (capture-cps-trace proc args))
         (nodes (extract-cps-nodes trace-data))
         (edges (extract-cps-edges trace-data)))
    (string-append
     "graph TD\n"
     "    classDef cpsNode fill:#faa,stroke:#333,stroke-width:2px\n"
     "    classDef contNode fill:#afa,stroke:#333,stroke-width:2px\n"
     (generate-node-definitions nodes)
     (generate-edge-definitions edges))))

(define (capture-cps-trace proc args)
  "Capture CPS execution trace"
  (call-with-output-string
   (lambda (port)
     (parameterize ((current-output-port port))
       (call-with-trace
        (lambda ()
          (apply proc args))
        #:calls? #t
        #:width 120)))))

(define (extract-cps-nodes trace-data)
  "Extract nodes from CPS trace"
  ;; Parse trace data to identify unique function calls
  '()) ; Placeholder

(define (extract-cps-edges trace-data)
  "Extract edges from CPS trace"
  ;; Parse trace data to identify call relationships
  '()) ; Placeholder

(define (generate-node-definitions nodes)
  "Generate Mermaid node definitions"
  (string-join
   (map (lambda (node)
          (format #f "    ~a[\"~a\"]:::~a\n"
                  (node-id node)
                  (node-label node)
                  (node-class node)))
        nodes)
   ""))

(define (generate-edge-definitions edges)
  "Generate Mermaid edge definitions"
  (string-join
   (map (lambda (edge)
          (format #f "    ~a --> ~a\n"
                  (edge-from edge)
                  (edge-to edge)))
        edges)
   ""))

;;;; Interactive Capture

(define (capture-evaluation-trace thunk)
  "Capture complete evaluation trace with timing"
  (let ((start-time (current-time))
        (trace-output '())
        (stack-snapshots '()))
    
    ;; Install hooks to capture evaluation
    (add-hook! (vm-trace-level) 
               (lambda (level)
                 (set! trace-output 
                       (cons (format #f "Level ~a at ~a" 
                                    level 
                                    (- (current-time) start-time))
                             trace-output))))
    
    ;; Execute with monitoring
    (let ((result (thunk)))
      
      ;; Remove hooks
      (reset-hook! (vm-trace-level))
      
      ;; Return comprehensive trace data
      `((result . ,result)
        (duration . ,(- (current-time) start-time))
        (trace . ,(reverse trace-output))
        (snapshots . ,stack-snapshots)))))

;;;; Call Graph Generation

(define (create-call-graph proc)
  "Create call graph for procedure"
  (let ((graph (analyze-procedure proc)))
    (string-append
     "digraph CallGraph {\n"
     "    rankdir=LR;\n"
     "    node [shape=box];\n"
     (format-graph-nodes graph)
     (format-graph-edges graph)
     "}\n")))

(define (analyze-procedure proc)
  "Analyze procedure to extract call graph"
  ;; This would need actual procedure analysis
  '((nodes . ())
    (edges . ())))

(define (format-graph-nodes graph)
  "Format nodes for DOT output"
  (string-join
   (map (lambda (node)
          (format #f "    \"~a\" [label=\"~a\"];\n"
                  (car node)
                  (cdr node)))
        (assoc-ref graph 'nodes))
   ""))

(define (format-graph-edges graph)
  "Format edges for DOT output"
  (string-join
   (map (lambda (edge)
          (format #f "    \"~a\" -> \"~a\";\n"
                  (car edge)
                  (cdr edge)))
        (assoc-ref graph 'edges))
   ""))

;;;; Helper definitions for node/edge structures

(define (node-id node) (car node))
(define (node-label node) (cadr node))
(define (node-class node) (caddr node))
(define (edge-from edge) (car edge))
(define (edge-to edge) (cadr edge))

;;;; Frame utilities

(define (frame-procedure-name frame)
  "Safe extraction of procedure name from frame"
  (catch #t
    (lambda ()
      (let ((proc (frame-procedure frame)))
        (if (program? proc)
            (program-name proc)
            #f)))
    (lambda _ #f)))

(define (frame-arguments frame)
  "Safe extraction of arguments from frame"
  (catch #t
    (lambda ()
      (frame-arguments frame))
    (lambda _ '())))