#lang racket

;; in this file we represent functions that modify elements of the tree

(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (make-index items (index-name "i"))
  (for/fold
    ((res empty))
    ((item items) (idx (in-naturals 1)))
    (pushr
      res
      (hash-union
        (hash index-name idx)
        item))))

; {w,w<r,w>r:first-existed}
(define (first-existed (fallback-value #f))
  (λ args
    (let ((existed (filter-not nil? args)))
      (if (empty? existed)
        fallback-value
        (first existed)))))

(define (add-type val list-of-hashes #:attr (attr 'type))
  (map
    (λ (x) (hash-union (hash attr val) x))
    list-of-hashes))
