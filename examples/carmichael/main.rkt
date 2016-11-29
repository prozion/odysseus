#lang racket

(require "../../utils/seqs.rkt")
(require "../../utils/base.rkt")

;; Fermat's little theorem illustration for different numbers from 1 to 1000
;; with Carmichael numbers (at least 561)
;; inspired by SICP

; theorem: a^n % n == a for any a < n
(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (sqr (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (getcolumn n)
  (map (λ (a) (= (expmod a n n) a)) (range 1 n)))

(define (ascii-viz-string n)
  (implode
    (map (λ (x) (if x "|" ".")) (getcolumn n))))

(define (find-quasi-carmichael upto (percent 0.5))
  (filter
    (λ (x)
      (in
        (* percent x)
        (- x 2)
        (length
            (filter
              true?
              (getcolumn x)))))
    (range 10 upto)))
