#lang racket

(provide (all-defined-out))

(define (vector-ref* v index)
  (vector-ref
    v
    (if (< index 0)
      (+ (vector-length v) index)
      index)))
