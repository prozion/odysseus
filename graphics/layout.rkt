#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

;; general functions for layout calculations

(define (svg/translate x y)
  (str "translate(" x " " y ")"))

(define (svg/rotate ang x y)
  (format "rotate(~a ~a ~a)" ang x y))
