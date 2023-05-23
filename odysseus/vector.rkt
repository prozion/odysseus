#lang racket

(provide (all-defined-out))

(define empty-vector (make-vector 0))

(define (vector-ref* v index)
  (vector-ref
    v
    (if (< index 0)
      (+ (vector-length v) index)
      index)))

(define (vector-conj v . els)
  (vector-append v (apply vector els)))
