#lang racket

(require "controls.rkt")
(require "base.rkt")
(require "seqs.rkt")
(require "alist.rkt")
(require "math.rkt")

(provide (all-defined-out))

(define-poly (s/median args)
  (let* ((sorted (sort args <))
        (l (length args))
        (i1 (quotient l 2))
        (i2 (inc i1)))
    (if (even? l)
      (avg (nth sorted i1) (nth sorted i2))
      (nth sorted i2))))

(define-poly (s/mode args)
  (let* ((freqs
          (map
            (λ (x) (list x (length (filter (curry = x) args))))
            args))
        (freqs (uniques freqs))
        (freqs (sort freqs (λ (a b) (> (second a) (second b))))))
    (caar freqs)))

; how to incorporate it to s/mode so the following is possible: (s/mode args #:epsilon (epsilon 0))?
(define (s/mode-epsilon arglst (epsilon 0))
  (let* ((freqs
          (map
            (λ (x) (list x (length (filter (λ (y) (<= (abs (- x y)) epsilon)) arglst))))
            arglst))
        (freqs (uniques freqs))
        (freqs (sort freqs (λ (a b) (> (second a) (second b))))))
    (caar freqs)))

; list -> list of pairs
(define (frequency seq)
  (for/fold
    ((s (list)))
    ((i seq))
    (clist-add
      s
      (cons i 1)
      (λ (v1 v2) (+ v1 v2)))))
