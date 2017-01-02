#lang racket

(module+ test

  (require rackunit)
  (require "../stat.rkt")

  (check-= (avg '(1 2 3 4 5)) 3 0)
  (check-= (avg '(1 1 1 1 1 10)) 15/6 0)
)
