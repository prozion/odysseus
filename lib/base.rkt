#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define nil null)

(define % remainder)

(define (int a)
  (inexact->exact (floor a)))

(define (fract a)
  (- a (int a)))

(define dec sub1)
(define inc add1)

(define (sqr a)
  (* a a))

(define-macro (make-rounded-op op round-op)
  `(λ xs (,round-op (apply ,op xs))))

(define // (make-rounded-op / exact->inexact))
(define /r (make-rounded-op / exact-round))
(define /f (make-rounded-op / exact-floor))
(define /c (make-rounded-op / exact-ceiling))
(define *r (make-rounded-op * exact-round))
(define *f (make-rounded-op * exact-floor))
(define *c (make-rounded-op * exact-ceiling))

(define (true? x) (if (equal? #f x) #f #t))

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (rand n)
  (add1 (random n)))

(define nil?
  (λ (v) (or
            (null? v)
            (void? v)
            (and (hash? v) (empty? (hash-keys v)))
            (and (string? v) (equal? v ""))
            (false? v))))

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

(define notnil? (not-> nil?))

(define znil?
  (λ (v) (or
            (nil? v)
            (and
              (number? v)
              (= v 0)))))

(define notznil? (not-> znil?))

;; filtering
(define (clean f xs)
  ;(filter (λ (x) (not (f x))) xs))
  (filter-not f xs))

(define (in a b x)
  (<= a x b))

(define inii in)

(define (inee a b x)
  (< a x b))

(define (inei a b x)
  (and (< a x) (<= x b)))

(define (inie a b x)
  (and (<= a x) (< x b)))
