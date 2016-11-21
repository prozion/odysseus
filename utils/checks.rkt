#lang racket

(provide (all-defined-out))

(define (check-hash-equal/simple? h1 h2)
  (equal?
    (sort
      (hash-values h1)
      <)
    (sort
      (hash-values h2)
      <)))

(define (check-hash-equal? h1 h2)
    (and
      (for/fold ((a #t)) (([k v] h2))
          (and a (equal? (hash-ref h1 k #f) v)))
      (for/fold ((a #t)) (([k v] h1))
          (and a (equal? (hash-ref h2 k #f) v)))))
