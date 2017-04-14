#lang racket

(module+ test

  (require rackunit)
  (require "../optimize.rkt")

  (check-equal? (opt/uniques '(8 1 2 3 3 4 5 2 10 2)) '(8 1 2 3 4 5 10))

  ;(check-equal? (opt/flatten '((8 1) ((2)) (3 (3 (4 5))) 2 10 2 '())) '(8 1 2 3 3 4 5 2 10 2))
)
