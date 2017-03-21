#lang racket

(module+ test

  (require rackunit)
  (require "../interval.rkt")

  (check-equal? (+c 4 4 5) 3)
  (check-equal? (+c 3 2 8) 5)
  (check-equal? (+c 17 20 4) 1)

  (check-true (>plain 10 5))
  (check-true (>plain 4 3))
  (check-true (>plain 2 11))

  ;(check-equal? (fractize 1 16 3) '(5 10 15))
  ;(check-equal? (fractize 0 100 10) '(0 10 20 30 40 50 60 70 80 90))
  ;(check-equal? (fractize 0 100 4) '(0 30 60 90))

  ;(check-equal? (fractize-3 1 16 1/4) '(0 5 10 15 20))
)
