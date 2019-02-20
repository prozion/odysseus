#lang racket

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

(module+ test

  (require rackunit)
  (require "checks.rkt")

  (check-same-elements? (permutations '(a b c)) '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a)))

)
