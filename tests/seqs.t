#lang racket

(module+ test

  (require rackunit)
  (require "../lib/seqs.rkt")

  (check-equal? (len "") 0)
  (check-equal? (len "Oslo is a capital of Great Britain") 34)
  (check-equal? (len '()) 0)
  (check-equal? (len '(1 2 3 4 5)) 5)

  (check-equal? (join '("a" "b" "c" "d") "-") "a-b-c-d")

  (check-equal? (split "") '())
  (check-equal? (split "Oslo") '("O" "s" "l" "o"))

  (check-equal? (nth "" 10) "")
  (check-equal? (nth "Oslo god morgen" 0) "O")
  (check-equal? (nth "Oslo god morgen" 5) "g")
  (check-equal? (nth "ost og skinke" -1) "e")
  (check-equal? (nth '() 10) null)
  (check-equal? (nth '(0 1 2 3 4 5) 0) 0)
  (check-equal? (nth '(0 1 2 3 4 5) 3) 3)
  (check-equal? (nth '(0 1 2 3 4 5) -1) 5)
)
