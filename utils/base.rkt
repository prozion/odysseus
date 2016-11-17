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

(define (*r . xs)
  (exact-round (apply * xs)))

(define (*f . xs)
  (exact-floor (apply * xs)))

(define (*c . xs)
  (exact-ceiling (apply * xs)))

(define (true? x) x)

(define (in a b x)
  (<= a x b))

(define inii in)

(define (inee a b x)
  (< a x b))

(define (inei a b x)
  (and (< a x) (<= x b)))

(define (inie a b x)
  (and (<= a x) (< x b)))

(define nil?
  (λ (v) (or (null? v) (and (string? v) (equal? v "")))))

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (clean f xs)
  (filter (λ (x) (not (f x))) xs))

(define (rand n)
  (add1 (random n)))

(define (type? x)
  (cond
    ((number? x) 'number)
    ((string? x) 'string)
    ((list? x) 'list)
    ((pair? x) 'pair)
    ((char? x) 'char)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    ((syntax? x) 'syntax)
    ((vector? x) 'vector)
    ((hash? x) 'hash) ; STX hash?
    ((path? x) 'path) ; STX path?
    (else #f)))

; cyclic addition (e.g. for finding contrast values on color circle)
(define (+c a b base)
  (let ((factor
          (if (or (inee 0 1 a) (inee 0 1 b))
            (/ 1 (min a b))
            1)))
    (/
      (remainder
        (exact-round (* factor (+ a b)))
        base)
      factor)))
