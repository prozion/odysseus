#lang racket

(require "../math.rkt")
(require rackunit)

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

(check-same-elements? (permutations '(a b c)) '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a)))

(check-equal? (f! 0) 1)
(check-equal? (f! 1) 1)
(check-equal? (f! 5) (* 1 2 3 4 5))

(check-equal? (a-kn 3 5) 60)

(check-equal? (c-kn 3 5) 10)
(check-equal? (c-kn 69 120) 25202394358996989831281417065516920)
