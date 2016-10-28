#lang racket

(provide (all-defined-out))

(define debug
  (lambda args (apply string-append
    (map
      (lambda (el)
        (cond
          ((number? el) (number->string el))
          ((list? el) (list->string el)) ; list of chars to string
          (else el)))
      args))))

(define (true? x) x)

(define (in a b x)
  (<= a x b))

(define nil?
  (λ (v) (or (null? v) (and (string? v) (equal? v "")))))

(define (!= a b)
  (not (= a b)))
