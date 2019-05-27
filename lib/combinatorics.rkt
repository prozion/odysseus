#lang racket

(require "base.rkt")
(require "seqs.rkt")
(require "type.rkt")
(require "debug.rkt")

(provide (all-defined-out))

(define (permutations alst)
  (cond
    ((empty? alst) empty)
    ((one-element? alst) (list alst))
    (else (for/fold
            ((res empty))
            ((i (range 1 (+ 1 (length alst)))))
            (append
              (map
                  (λ (x) (pushl x (nth alst i)))
                  (permutations (remove alst i)))
              res)))))

(define-catch (! n)
  (cond
    ((or (equal? n 0) (equal? n 1)) 1)
    ((> n 1) (* n (! (- n 1))))
    (else (errorf "Wrong number for factorial: ~a" n))))

(define (a-kn k n)
  (/
    (! n)
    (! (- n k))))

(define (c-kn k n)
  (/
    (! n)
    (! (- n k))
    (! k)))

(module+ test

  (require rackunit)
  (require "checks.rkt")

  (check-same-elements? (permutations '(a b c)) '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a)))

  (check-equal? (! 0) 1)
  (check-equal? (! 1) 1)
  (check-equal? (! 5) (* 1 2 3 4 5))

  (check-equal? (a-kn 3 5) 60)

  (check-equal? (c-kn 3 5) 10)
  (check-equal? (c-kn 69 120) 25202394358996989831281417065516920)
)
