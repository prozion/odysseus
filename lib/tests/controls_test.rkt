#lang racket

(module+ test

  (require rackunit)
  (require "../controls.rkt")

  (check-equal? (zor 3) 3)
  (check-equal? (zor 0 (- 2 2) (/ 0 5) 10 0) 10)

  (check-equal? (gen 1 5) '(1 1 1 1 1))
)
