#lang racket

(require "base.rkt")
(require "seqs.rkt")

(provide (all-defined-out))

(define PI 3.1415926535897932)
(define π PI)

(define (sqr a)
  (* a a))

(define (avg lst)
  (/ (apply + lst) (length lst)))

(define (median-avg lst)
  lst)

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
