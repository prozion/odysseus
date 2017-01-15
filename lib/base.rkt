#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define nil null)

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
  (exact-floor (/ a b)))

(define (/c a b)
  (exact-ceiling (/ a b)))

(define (*r . xs)
  (exact-round (apply * xs)))

(define (*f . xs)
  (exact-floor (apply * xs)))

(define (*c . xs)
  (exact-ceiling (apply * xs)))

(define (true? x) x)

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (rand n)
  (add1 (random n)))

(define (lg x a)
  (/ (log x) (log a)))

(define-macro (f-> f)
  `(λ preds
    (λ (argument)
      (let ((reslist (map (λ (x) (x argument)) preds)))
        (cond
          ((null? (cdr reslist)) (,f (car reslist)))
          (else
            (foldr
              (λ (a b) (,f a b))
              (car reslist)
              (cdr reslist)
              )))))))

(define and-> (f-> and))
(define or-> (f-> or))

(define (not-> f)
  (λ (argument)
    (not (f argument))))

(define nil?
  (λ (v) (or
            (null? v)
            (void? v)
            (and (hash? v) (empty? (hash-keys v)))
            (and (string? v) (equal? v ""))
            (false? v))))

(define notnil? (not-> nil?))

(define znil?
  (λ (v) (or (nil? v) (= v 0))))    

;; filtering
(define (clean f xs)
  ;(filter (λ (x) (not (f x))) xs))
  (filter-not f xs))
