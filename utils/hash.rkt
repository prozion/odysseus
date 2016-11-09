#lang racket

(provide (all-defined-out))

(define (map/hash f h)
  (for/hash (((k v) (in-hash h))) (values k (f v))))

(define (hash-length h)
  (length (hash-keys h)))

(define (print-hash format-str h)
  (for/fold
    ([res ""])
    ([(k v) (in-hash h)])
    (string-append res (format format-str k v))))
