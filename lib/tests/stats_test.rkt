#lang racket

(module+ test

  (require rackunit)
  (require "../stats.rkt")
  
  (check-equal? (s/median 1 2 3 4 5) 3)
  (check-equal? (s/median 1 1 10 1 1) 1)
  (check-equal? (s/median '(1 1 10 1 1)) 1)
  (check-equal? (s/median '(2 4 1 6 100)) 4)
  (check-= (s/median '(2 4 1 30 6 100)) 5 0)

(check-equal? (s/mode 1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 4 15 3 3 3 1 7) 3)
(check-equal? (s/mode '(1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 2 2 2 2 4 15 3 3 3 1 7)) 2)

  (check-equal? (s/mode 1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 4 15 3 3 3 1 7) 3)
  (check-equal? (s/mode '(1 2 2 3 2 1 10 4 8 8 3 8 8 9 9 10 11 3 3 2 2 2 2 4 15 3 3 3 1 7)) 2)

  (check-equal? (frequency '(1 2 3 1 1 3 2 7 4 9 10 45 3 8 44 4 5))
                '((1 . 3) (2 . 2) (3 . 3) (7 . 1) (4 . 2) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1)))
)
