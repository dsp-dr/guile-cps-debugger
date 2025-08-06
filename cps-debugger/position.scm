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

(define-module (cps-debugger position)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)  ; vectors
  #:use-module (ice-9 match)
  #:export (make-position
            position?
            position->list
            list->position
            position-empty?
            position-depth
            position-parent
            position-child
            position-sibling
            position-equal?
            position-navigate
            position-cache-make
            position-cache-get
            position-cache-put!))

;;; Commentary:
;;;
;;; Efficient position handle abstraction for CPS term navigation.
;;; Replaces list-based positions with optimized vector representation.
;;;
;;; Code:

(define-record-type <position>
  (make-position* path cache-key)
  position?
  (path position-path)         ; Vector of indices
  (cache-key position-cache-key set-position-cache-key!))

(define (make-position . indices)
  "Create a position from indices."
  (make-position* (list->vector indices) #f))

(define (position->list pos)
  "Convert position to list for compatibility."
  (vector->list (position-path pos)))

(define (list->position lst)
  "Convert list to position."
  (make-position* (list->vector lst) #f))

(define (position-empty? pos)
  "Check if position is at root."
  (zero? (vector-length (position-path pos))))

(define (position-depth pos)
  "Get the depth of a position."
  (vector-length (position-path pos)))

(define (position-parent pos)
  "Get parent position efficiently."
  (let ((path (position-path pos))
        (len (vector-length (position-path pos))))
    (if (zero? len)
        pos  ; Already at root
        (make-position* (vector-copy path 0 (- len 1)) #f))))

(define (position-child pos index)
  "Get child position at INDEX."
  (let* ((path (position-path pos))
         (len (vector-length path))
         (new-path (make-vector (+ len 1))))
    (vector-copy! new-path 0 path)
    (vector-set! new-path len index)
    (make-position* new-path #f)))

(define (position-sibling pos offset)
  "Get sibling position with OFFSET."
  (let* ((path (position-path pos))
         (len (vector-length path)))
    (if (zero? len)
        pos  ; No siblings at root
        (let ((new-path (vector-copy path))
              (last-idx (- len 1)))
          (vector-set! new-path last-idx 
                      (+ (vector-ref path last-idx) offset))
          (make-position* new-path #f)))))

(define (position-equal? pos1 pos2)
  "Check if two positions are equal."
  (vector= = (position-path pos1) (position-path pos2)))

(define (position-navigate term pos)
  "Navigate to position in TERM with proper error handling.
Returns the sub-term at position, or #f if position is invalid."
  (catch #t
    (lambda ()
      (let loop ((term term)
                 (indices (position-path pos))
                 (idx 0))
        (cond
         ((>= idx (vector-length indices)) term)
         ((not (pair? term)) 
          (error 'position-error "Cannot navigate into non-pair term" term))
         (else
          (let ((index (vector-ref indices idx)))
            (if (and (list? term) (< index (length term)))
                (loop (list-ref term index) indices (+ idx 1))
                (error 'position-error 
                       "Index ~a out of bounds for term of length ~a" 
                       index (length term))))))))
    (lambda (key . args)
      ;; Return #f on navigation errors for graceful degradation
      #f)))

;; Position cache for frequently accessed positions
(define-record-type <position-cache>
  (make-position-cache* table size max-size)
  position-cache?
  (table cache-table)
  (size cache-size set-cache-size!)
  (max-size cache-max-size))

(define* (position-cache-make #:key (max-size 1000))
  "Create a position cache with MAX-SIZE entries."
  (make-position-cache* (make-hash-table) 0 max-size))

(define (position-cache-get cache pos)
  "Get cached result for position."
  (hash-ref (cache-table cache) (position-path pos)))

(define (position-cache-put! cache pos value)
  "Cache a value for position."
  (let ((table (cache-table cache)))
    (unless (hash-ref table (position-path pos))
      (when (>= (cache-size cache) (cache-max-size cache))
        ;; Simple eviction: clear half the cache
        (hash-clear! table)
        (set-cache-size! cache 0))
      (hash-set! table (position-path pos) value)
      (set-cache-size! cache (+ 1 (cache-size cache))))
    value))