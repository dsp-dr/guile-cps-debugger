;;; continuation.scm --- Continuation chain analysis for CPS debugger
;;; Commentary:
;;;
;;; This module provides facilities for analyzing continuation chains in
;;; CPS-transformed code. It helps understand the flow of continuations,
;;; their nesting depth, and memory characteristics.
;;;
;;; Code:

(define-module (cps-debugger analysis continuation)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (make-continuation-info
            continuation-info?
            continuation-depth
            continuation-closures
            continuation-memory
            analyze-continuation-chain
            build-continuation-tree
            find-continuation-cycles
            continuation-chain-statistics
            format-continuation-chain))

;;; Continuation info record
(define-record-type <continuation-info>
  (make-continuation-info depth closures memory metadata)
  continuation-info?
  (depth continuation-depth)
  (closures continuation-closures)
  (memory continuation-memory)
  (metadata continuation-metadata))

;;; Continuation node for tree building
(define-record-type <continuation-node>
  (make-continuation-node id parent children proc args timestamp)
  continuation-node?
  (id node-id)
  (parent node-parent set-node-parent!)
  (children node-children set-node-children!)
  (proc node-proc)
  (args node-args)
  (timestamp node-timestamp))

;;; Main analysis function
(define (analyze-continuation-chain trace-data)
  "Analyze a continuation chain from trace data."
  (let* ((chain (extract-continuation-chain trace-data))
         (depth (calculate-chain-depth chain))
         (closures (extract-closures chain))
         (memory (estimate-memory-usage chain))
         (metadata (compute-metadata chain)))
    (make-continuation-info depth closures memory metadata)))

;;; Extract continuation chain from trace data
(define (extract-continuation-chain trace-data)
  "Extract continuation calls from trace data."
  (filter (lambda (entry)
            (and (assq-ref entry 'procedure)
                 (let ((args (assq-ref entry 'arguments)))
                   (and (not (null? args))
                        (procedure? (last args))))))
          trace-data))

;;; Calculate chain depth
(define (calculate-chain-depth chain)
  "Calculate the maximum depth of the continuation chain."
  (let loop ((entries chain)
             (current-depth 0)
             (max-depth 0))
    (if (null? entries)
        max-depth
        (let* ((entry (car entries))
               (is-continuation-call? (is-cps-call? entry))
               (new-depth (if is-continuation-call?
                            (+ current-depth 1)
                            current-depth)))
          (loop (cdr entries)
                new-depth
                (max max-depth new-depth))))))

;;; Check if entry is a CPS call
(define (is-cps-call? entry)
  "Check if trace entry represents a CPS call."
  (let ((args (assq-ref entry 'arguments)))
    (and args
         (not (null? args))
         (procedure? (last args)))))

;;; Extract closures from chain
(define (extract-closures chain)
  "Extract closure information from continuation chain."
  (map (lambda (entry)
         (let ((proc (assq-ref entry 'procedure))
               (args (assq-ref entry 'arguments)))
           `((procedure . ,proc)
             (arity . ,(length args))
             (has-continuation . ,(and (not (null? args))
                                      (procedure? (last args))))
             (free-variables . ,(estimate-free-variables proc)))))
       chain))

;;; Estimate free variables (simplified)
(define (estimate-free-variables proc)
  "Estimate number of free variables in procedure."
  ;; This is a simplified estimation
  ;; In a real implementation, we'd inspect the procedure's code
  0)

;;; Estimate memory usage
(define (estimate-memory-usage chain)
  "Estimate memory usage of continuation chain."
  (let ((closure-size 64)  ; Estimated bytes per closure
        (frame-size 128)   ; Estimated bytes per frame
        (num-closures (length chain)))
    `((total-closures . ,num-closures)
      (estimated-closure-memory . ,(* num-closures closure-size))
      (estimated-frame-memory . ,(* num-closures frame-size))
      (total-estimated . ,(* num-closures (+ closure-size frame-size))))))

;;; Compute metadata
(define (compute-metadata chain)
  "Compute additional metadata about the continuation chain."
  (let ((timestamps (filter-map (cut assq-ref <> 'time) chain)))
    `((length . ,(length chain))
      (start-time . ,(if (null? timestamps) #f (car timestamps)))
      (end-time . ,(if (null? timestamps) #f (last timestamps)))
      (unique-procedures . ,(length (delete-duplicates 
                                    (map (cut assq-ref <> 'procedure) chain)))))))

;;; Build continuation tree
(define (build-continuation-tree trace-data)
  "Build a tree structure from continuation chain."
  (let ((nodes '())
        (id-counter 0))
    
    (define (make-node entry parent-id)
      (set! id-counter (+ id-counter 1))
      (make-continuation-node 
       id-counter
       parent-id
       '()
       (assq-ref entry 'procedure)
       (assq-ref entry 'arguments)
       (assq-ref entry 'time)))
    
    ;; Build tree from trace data
    (let loop ((entries trace-data)
               (current-parent #f)
               (tree #f))
      (if (null? entries)
          tree
          (let* ((entry (car entries))
                 (node (make-node entry current-parent)))
            (set! nodes (cons node nodes))
            (when current-parent
              (let ((parent (find (lambda (n) (= (node-id n) current-parent)) nodes)))
                (when parent
                  (set-node-children! parent (cons node (node-children parent))))))
            (if (not tree)
                (loop (cdr entries) (node-id node) node)
                (loop (cdr entries) (node-id node) tree)))))))

;;; Find cycles in continuation chain
(define (find-continuation-cycles chain)
  "Detect cycles in continuation chain."
  (let ((seen '())
        (cycles '()))
    
    (define (procedure-equal? p1 p2)
      ;; Simplified procedure comparison
      (eq? p1 p2))
    
    (for-each
     (lambda (entry)
       (let ((proc (assq-ref entry 'procedure)))
         (cond
          ((member proc seen procedure-equal?)
           (set! cycles (cons proc cycles)))
          (else
           (set! seen (cons proc seen))))))
     chain)
    
    cycles))

;;; Calculate statistics
(define (continuation-chain-statistics chain-info)
  "Calculate statistics for continuation chain."
  (let ((depth (continuation-depth chain-info))
        (closures (continuation-closures chain-info))
        (memory (continuation-memory chain-info))
        (metadata (continuation-metadata chain-info)))
    `((max-depth . ,depth)
      (total-continuations . ,(assq-ref metadata 'length))
      (unique-procedures . ,(assq-ref metadata 'unique-procedures))
      (average-arity . ,(if (null? closures)
                           0
                           (/ (apply + (map (cut assq-ref <> 'arity) closures))
                              (length closures))))
      (memory-per-continuation . ,(if (zero? (assq-ref metadata 'length))
                                    0
                                    (/ (assq-ref memory 'total-estimated)
                                       (assq-ref metadata 'length)))))))

;;; Format continuation chain for display
(define (format-continuation-chain chain-info)
  "Format continuation chain info for display."
  (let ((stats (continuation-chain-statistics chain-info)))
    (format #f "Continuation Chain Analysis:
  Maximum Depth: ~a
  Total Continuations: ~a
  Unique Procedures: ~a
  Average Arity: ~,2f
  Estimated Memory: ~a bytes
  Memory per Continuation: ~,2f bytes"
            (assq-ref stats 'max-depth)
            (assq-ref stats 'total-continuations)
            (assq-ref stats 'unique-procedures)
            (assq-ref stats 'average-arity)
            (assq-ref (continuation-memory chain-info) 'total-estimated)
            (assq-ref stats 'memory-per-continuation))))

;;; Helper function
(define (last lst)
  "Return the last element of LST."
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))