#lang racket

(require "base.rkt")

(provide (all-defined-out))

(define (avg lst)
  (/ (apply + lst) (length lst)))

(define (median-avg lst)
  lst)
