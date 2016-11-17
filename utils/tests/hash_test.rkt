#lang racket

(module+ test

  (require rackunit)
  (require "../hash.rkt")

  (define h (hash 'a (hash 'aa 10 'ab 20) 'b (hash 'ba (hash 'baa 300 'bab 30))))

  (check-equal? (hash-path (hash 'a 1 'b 10 'c 100) 'c) 100)
  (check-equal? (hash-path h 'b 'ba 'bab) 30)
)
