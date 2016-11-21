#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define % remainder)

(define (int a)
  (inexact->exact (floor a)))

(define dec sub1)

(define inc add1)

(define (// a b)
  (exact->inexact (/ a b)))

(define (/r a b)
  (exact-round (/ a b)))

(define (/f a b)
  (exact-floor (/ a b))) ; STX exact-floor ~exact-round

(define (/c a b)
  (exact-ceiling (/ a b))) ; STX exact-ceiling ~exact-round

(define (*r . xs)
  (exact-round (apply * xs)))

(define (*f . xs)
  (exact-floor (apply * xs)))

(define (*c . xs)
  (exact-ceiling (apply * xs)))

(define (true? x) x)

(define nil?
  (λ (v) (or
            (null? v)
            (void? v)
            (and (string? v) (equal? v ""))
            (false? v)))) ; STX false?

(define znil?
  (λ (v) (or (nil? v) (= v 0))))

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (clean f xs)
  (filter (λ (x) (not (f x))) xs))

(define (rand n)
  (add1 (random n)))

(define (lg x a)
  (/ (log x) (log a)))
