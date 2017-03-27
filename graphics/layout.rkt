#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

;; geometric functions

(define (distance x1 y1 x2 y2)
  (sqrt (+
          (sqr (- x2 x1))
          (sqr (- y2 y1)))))

;; general functions for layout control in svg

(define (svg/translate x y)
  (str "translate(" x " " y ")"))

(define (svg/rotate ang x y)
  (format "rotate(~a ~a ~a)" ang x y))
