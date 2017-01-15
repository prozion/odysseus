#lang racket

(require "base.rkt")

(provide (all-defined-out))

(define PI 3.1415926535897932)

(define (avg lst)
  (/ (apply + lst) (length lst)))

(define (median-avg lst)
  lst)

(define (grad->rad x) (* PI (/ x 180))) ; or use degrees->radians

(define (rad->grad x) (* 180 (/ x PI))) ; or use radians->degrees
