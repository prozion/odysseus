#lang racket

(module+ test

  (require rackunit)
  (require "../math.rkt")

  (check-= (avg 1 2 3 4 5) 3 0)
  (check-= (avg 1 1 1 1 1 10) 15/6 0)

  (check-= (gavg 1 2 3 4 5) 24 0)

  (check-= (avg 1 2 3 4 5) 3 0)
  (check-= (avg 1 1 1 1 1 10) 15/6 0)

  (check-= (gavg 1 2 3 4 5) 24 0)

  (check-equal? (hex->dec "0") 0)
  (check-equal? (hex->dec "a2") 162)

  (check-equal? (dec->hex 0) "0")
  (check-equal? (dec->hex 162) "a2")

  (check-equal? (accumulate '(1 2 3 4 5)) '(1 3 6 10 15))
  (check-equal? (accumulate '(1 2 3 4 5) #:op *) '(1 2 6 24 120))

  (check-= (lg 2 4) 2 1e-10)
  (check-= (lg 10 1e100) 100 1e-10)
)
