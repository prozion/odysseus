#lang racket

(require "base.rkt")
(require "controls.rkt")
(require "seqs.rkt")
(require "alist.rkt")

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
      (else (str (dec->hex-r (quotient v 16)) (next-hex (% v 16))))))
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

; sum up sequence
; (sum-seq (lambda (n) (/ 1.0 (* n n n))) 1e7) -> Apery's constant
(define (sum-seq f k (s 0))
  (cond
    ((= k 0) s)
    (else (sum-seq f (dec k) (+ s (f k))))))

(define (reqsum f k (s 0))
  (cond
    ((= 0 k) s)
    (else (reqsum f (dec k) (+ s (f s))))))

(define (lg a x)
  (/ (log x) (log a)))

(define (sec x)
  (/ 1 (cos x)))

(define (distance x1 y1 x2 y2)
  (sqrt (+
          (sqr (- x2 x1))
          (sqr (- y2 y1)))))
