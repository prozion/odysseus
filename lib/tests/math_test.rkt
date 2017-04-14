#lang racket

(module+ test

  (require rackunit)
  (require "../math.rkt")

  (check-= (avg 1 2 3 4 5) 3 0)
  (check-= (avg 1 1 1 1 1 10) 15/6 0)

  (check-= (gavg 1 2 3 4 5) 24 0)

  (check-equal? (s/median 1 2 3 4 5) 3)
  (check-equal? (s/median 1 1 10 1 1) 1)
  (check-equal? (s/median '(1 1 10 1 1)) 1)
  (check-equal? (s/median '(2 4 1 6 100)) 4)
  (check-= (s/median '(2 4 1 30 6 100)) 5 0)

  (check-equal? (s/mode 1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 4 15 3 3 3 1 7) 3)
  (check-equal? (s/mode '(1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 2 2 2 2 4 15 3 3 3 1 7)) 2)

  (check-equal? (hex->dec "0") 0)
  (check-equal? (hex->dec "a2") 162)

  (check-equal? (dec->hex 0) "0")
  (check-equal? (dec->hex 162) "a2")

  (check-equal? (accumulate '(1 2 3 4 5)) '(1 3 6 10 15))
  (check-equal? (accumulate '(1 2 3 4 5) #:op *) '(1 2 6 24 120))
)
