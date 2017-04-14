#lang racket

(require "base.rkt")
(require "controls.rkt")
(require "seqs.rkt")

(require compatibility/defmacro)

(provide (all-defined-out))

(define PI 3.1415926535897932)
(define π PI)

;(define-macro (avg-weighted name op)
;  `(define-poly (,name args)
;      (/ (apply ,op args) (length args))))
;
;(avg-weighted avg +)
;(avg-weighted gavg *)

(define (avg . args)
  (/ (apply + args) (length args)))

(define (gavg . args)
  (/ (apply * args) (length args)))

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

; TODO: understand how to incorporate it to s/mode so the following is possible: (s/mode args #:epsilon (epsilon 0))
(define (s/mode-epsilon arglst (epsilon 0))
  (let* ((freqs
          (map
            (λ (x) (list x (length (filter (λ (y) (<= (abs (- x y)) epsilon)) arglst))))
            arglst))
        (freqs (uniques freqs))
        (freqs (sort freqs (λ (a b) (> (second a) (second b))))))
    (caar freqs)))

(define (grad->rad x) (* PI (/ x 180))) ; or use degrees->radians

(define (rad->grad x) (* 180 (/ x PI))) ; or use radians->degrees

; "a2" -> 162
(define (hex->dec v)
  (string->number (str "#x" v)))

; 162 -> "a2"
(define  (dec->hex v)
  (define (next-hex v)
    (let ((hex "0123456789abcdef"))
      (nth hex (add1 (% v 16)))))
  (define (dec->hex-r v)
    (cond
      ((= v 0) "")
      (else (str (dec->hex-r (quotient v 16)) (next-hex (% v 16)) ))))
  (cond
    ((= v 0) "0")
    (else (dec->hex-r v))))

(define (accumulate lst #:op (op +))
  (cond
    ((null? (cdr lst)) lst)
    (else
      (foldl
        (λ (x x0)
          (pushr x0 (op (last x0) x)))
        (list (car lst))
        (cdr lst)))))
