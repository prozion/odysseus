#lang racket

(provide (all-defined-out))

(define (vector-ref* v index)
  (vector-ref
    v
    (if (< index 0)
      (+ (vector-length v) index)
      index)))

(define (vector-conj v . els)
  (vector-append v (apply vector els)))
