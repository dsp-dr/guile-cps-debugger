;;; memory.scm --- Memory profiling for CPS code
;;; Commentary:
;;;
;;; This module provides memory profiling capabilities specifically designed
;;; for analyzing memory usage patterns in CPS-transformed code. It focuses
;;; on closure allocation, heap usage, and detecting potential memory leaks
;;; in continuation chains.
;;;
;;; Code:

(define-module (cps-debugger analysis memory)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (profile-cps-memory
            analyze-closure-allocation
            detect-memory-leaks
            make-memory-report
            memory-report?
            report-summary
            report-details
            report-leaks
            format-memory-report
            compare-memory-snapshots
            track-allocation-sites))

;;; Memory report record
(define-record-type <memory-report>
  (make-memory-report-internal summary details leaks snapshots)
  memory-report?
  (summary report-summary)
  (details report-details)
  (leaks report-leaks)
  (snapshots report-snapshots))

;;; Memory snapshot record
(define-record-type <memory-snapshot>
  (make-memory-snapshot timestamp heap-size allocated gc-count closures)
  memory-snapshot?
  (timestamp snapshot-timestamp)
  (heap-size snapshot-heap-size)
  (allocated snapshot-allocated)
  (gc-count snapshot-gc-count)
  (closures snapshot-closures))

;;; Allocation site tracking
(define-record-type <allocation-site>
  (make-allocation-site location count size objects)
  allocation-site?
  (location site-location)
  (count site-count set-site-count!)
  (size site-size set-site-size!)
  (objects site-objects set-site-objects!))

;;; Global allocation tracking
(define *allocation-sites* (make-hash-table))
(define *tracking-enabled* #f)

;;; Main profiling function
(define (profile-cps-memory thunk)
  "Profile memory usage during CPS execution of THUNK."
  (let ((snapshots '())
        (start-time (current-time)))
    
    ;; Enable allocation tracking
    (set! *tracking-enabled* #t)
    (hash-clear! *allocation-sites*)
    
    ;; Take initial snapshot
    (gc)
    (let ((initial-snapshot (take-memory-snapshot)))
      (set! snapshots (cons initial-snapshot snapshots)))
    
    ;; Run the thunk with periodic snapshots
    (let ((result (with-memory-monitoring thunk 
                   (lambda (snapshot)
                     (set! snapshots (cons snapshot snapshots))))))
      
      ;; Take final snapshot
      (gc)
      (let ((final-snapshot (take-memory-snapshot)))
        (set! snapshots (cons final-snapshot snapshots)))
      
      ;; Disable tracking
      (set! *tracking-enabled* #f)
      
      ;; Analyze results
      (let* ((snapshots-ordered (reverse snapshots))
             (summary (analyze-memory-summary initial-snapshot final-snapshot))
             (details (analyze-memory-details snapshots-ordered))
             (leaks (detect-leaks snapshots-ordered)))
        (values result (make-memory-report-internal summary details leaks snapshots-ordered))))))

;;; Take memory snapshot
(define (take-memory-snapshot)
  "Take a snapshot of current memory state."
  (let ((gc-stats (gc-stats)))
    (make-memory-snapshot
     (current-time)
     (assq-ref gc-stats 'heap-size)
     (assq-ref gc-stats 'bytes-allocated)
     (assq-ref gc-stats 'gc-times)
     (count-live-closures))))

;;; Count live closures (simplified)
(define (count-live-closures)
  "Count approximate number of live closures."
  ;; This is a simplified version
  ;; Real implementation would walk the heap
  (let ((gc-stats (gc-stats)))
    (inexact->exact (/ (assq-ref gc-stats 'heap-allocated-words) 10))))

;;; Monitor memory during execution
(define (with-memory-monitoring thunk snapshot-callback)
  "Execute THUNK with periodic memory monitoring."
  (let ((monitor-thread #f)
        (monitoring #t)
        (result #f)
        (error #f))
    
    ;; Start monitoring thread
    (set! monitor-thread
          (call-with-new-thread
           (lambda ()
             (while monitoring
               (usleep 10000)  ; 10ms intervals
               (when *tracking-enabled*
                 (snapshot-callback (take-memory-snapshot)))))))
    
    ;; Execute thunk
    (catch #t
      (lambda ()
        (set! result (thunk)))
      (lambda args
        (set! error args)))
    
    ;; Stop monitoring
    (set! monitoring #f)
    (join-thread monitor-thread)
    
    (if error
        (apply throw error)
        result)))

;;; Analyze closure allocation
(define (analyze-closure-allocation snapshots)
  "Analyze closure allocation patterns from memory snapshots."
  (let ((allocations '()))
    
    (define (compare-snapshots s1 s2)
      (let ((closures-diff (- (snapshot-closures s2) (snapshot-closures s1)))
            (heap-diff (- (snapshot-heap-size s2) (snapshot-heap-size s1)))
            (time-diff (- (snapshot-timestamp s2) (snapshot-timestamp s1))))
        `((closures-allocated . ,closures-diff)
          (heap-growth . ,heap-diff)
          (allocation-rate . ,(if (zero? time-diff)
                                 0
                                 (/ closures-diff time-diff)))
          (bytes-per-closure . ,(if (zero? closures-diff)
                                  0
                                  (/ heap-diff closures-diff))))))
    
    ;; Analyze consecutive snapshots
    (let loop ((snaps snapshots)
               (results '()))
      (if (or (null? snaps) (null? (cdr snaps)))
          results
          (loop (cdr snaps)
                (cons (compare-snapshots (car snaps) (cadr snaps))
                      results))))))

;;; Detect memory leaks
(define (detect-memory-leaks snapshots)
  "Detect potential memory leaks from snapshot data."
  (detect-leaks snapshots))

(define (detect-leaks snapshots)
  "Analyze snapshots for potential memory leaks."
  (if (< (length snapshots) 3)
      '()
      (let* ((growth-rates (calculate-growth-rates snapshots))
             (suspicious-growth (filter (lambda (rate) (> rate 0.1)) growth-rates)))
        
        (map (lambda (idx rate)
               (let ((snap (list-ref snapshots idx)))
                 `((timestamp . ,(snapshot-timestamp snap))
                   (growth-rate . ,rate)
                   (heap-size . ,(snapshot-heap-size snap))
                   (severity . ,(cond ((> rate 0.5) 'high)
                                    ((> rate 0.2) 'medium)
                                    (else 'low))))))
             (iota (length suspicious-growth))
             suspicious-growth))))

;;; Calculate growth rates
(define (calculate-growth-rates snapshots)
  "Calculate heap growth rates between snapshots."
  (let loop ((snaps snapshots)
             (rates '()))
    (if (< (length snaps) 2)
        (reverse rates)
        (let* ((s1 (car snaps))
               (s2 (cadr snaps))
               (growth (- (snapshot-heap-size s2) (snapshot-heap-size s1)))
               (rate (/ growth (snapshot-heap-size s1))))
          (loop (cdr snaps) (cons rate rates))))))

;;; Analyze memory summary
(define (analyze-memory-summary initial-snapshot final-snapshot)
  "Create summary analysis from initial and final snapshots."
  `((initial-heap . ,(snapshot-heap-size initial-snapshot))
    (final-heap . ,(snapshot-heap-size final-snapshot))
    (heap-growth . ,(- (snapshot-heap-size final-snapshot)
                      (snapshot-heap-size initial-snapshot)))
    (total-allocated . ,(- (snapshot-allocated final-snapshot)
                          (snapshot-allocated initial-snapshot)))
    (gc-runs . ,(- (snapshot-gc-count final-snapshot)
                  (snapshot-gc-count initial-snapshot)))
    (closures-created . ,(- (snapshot-closures final-snapshot)
                           (snapshot-closures initial-snapshot)))
    (execution-time . ,(- (snapshot-timestamp final-snapshot)
                         (snapshot-timestamp initial-snapshot)))))

;;; Analyze memory details
(define (analyze-memory-details snapshots)
  "Create detailed analysis from all snapshots."
  `((snapshot-count . ,(length snapshots))
    (peak-heap-size . ,(apply max (map snapshot-heap-size snapshots)))
    (average-heap-size . ,(/ (apply + (map snapshot-heap-size snapshots))
                            (length snapshots)))
    (allocation-analysis . ,(analyze-closure-allocation snapshots))
    (gc-frequency . ,(calculate-gc-frequency snapshots))))

;;; Calculate GC frequency
(define (calculate-gc-frequency snapshots)
  "Calculate garbage collection frequency."
  (let ((gc-counts (map snapshot-gc-count snapshots))
        (timestamps (map snapshot-timestamp snapshots)))
    (if (< (length snapshots) 2)
        0
        (let ((total-gcs (- (last gc-counts) (car gc-counts)))
              (total-time (- (last timestamps) (car timestamps))))
          (if (zero? total-time)
              0
              (/ total-gcs total-time))))))

;;; Format memory report
(define (format-memory-report report)
  "Format memory report for display."
  (let ((summary (report-summary report))
        (details (report-details report))
        (leaks (report-leaks report)))
    (string-append
     (format #f "Memory Usage Report:
================================================================================
Initial Heap Size: ~:d bytes
Final Heap Size: ~:d bytes
Heap Growth: ~:d bytes (~,2f%)
Total Allocated: ~:d bytes
GC Runs: ~d
Closures Created: ~d
Execution Time: ~,3f seconds

Peak Heap Size: ~:d bytes
Average Heap Size: ~:d bytes
GC Frequency: ~,2f/second
"
             (assq-ref summary 'initial-heap)
             (assq-ref summary 'final-heap)
             (assq-ref summary 'heap-growth)
             (* 100.0 (/ (assq-ref summary 'heap-growth)
                        (assq-ref summary 'initial-heap)))
             (assq-ref summary 'total-allocated)
             (assq-ref summary 'gc-runs)
             (assq-ref summary 'closures-created)
             (assq-ref summary 'execution-time)
             (assq-ref details 'peak-heap-size)
             (inexact->exact (assq-ref details 'average-heap-size))
             (assq-ref details 'gc-frequency))
     
     (if (null? leaks)
         "\nNo memory leaks detected.\n"
         (format #f "\nPotential Memory Leaks Detected:
~{  - ~a: Growth rate ~,2f% (heap size: ~:d bytes)~%~}"
                 (map (lambda (leak)
                        (list (assq-ref leak 'timestamp)
                              (* 100 (assq-ref leak 'growth-rate))
                              (assq-ref leak 'heap-size)))
                      leaks))))))

;;; Compare memory snapshots
(define (compare-memory-snapshots snapshot1 snapshot2)
  "Compare two memory snapshots."
  `((heap-diff . ,(- (snapshot-heap-size snapshot2)
                    (snapshot-heap-size snapshot1)))
    (allocated-diff . ,(- (snapshot-allocated snapshot2)
                         (snapshot-allocated snapshot1)))
    (closures-diff . ,(- (snapshot-closures snapshot2)
                        (snapshot-closures snapshot1)))
    (gc-diff . ,(- (snapshot-gc-count snapshot2)
                  (snapshot-gc-count snapshot1)))
    (time-diff . ,(- (snapshot-timestamp snapshot2)
                    (snapshot-timestamp snapshot1)))))

;;; Track allocation sites
(define (track-allocation-sites proc args)
  "Track allocation site for debugging."
  (when *tracking-enabled*
    (let* ((key (cons proc args))
           (site (hash-ref *allocation-sites* key #f)))
      (if site
          (begin
            (set-site-count! site (+ 1 (site-count site)))
            (set-site-size! site (+ 64 (site-size site))))  ; Estimated size
          (hash-set! *allocation-sites* key
                     (make-allocation-site key 1 64 '()))))))