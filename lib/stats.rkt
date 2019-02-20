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

(module+ test

  (require rackunit)

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
