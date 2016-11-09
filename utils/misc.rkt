#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define % remainder)

(define (// a b)
  (exact->inexact (/ a b)))

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

; (gen (random 100) 10) -> '(1 34 50 7 80 62 58 91 10 8)
(define-macro (gen f size)
  `(let ((n ,size))
    (define (gen-r count)
      (cond
        ((= count 1) (list ,f))
        (else (cons ,f (gen-r (- count 1))))))
    (gen-r n)))

(define (rand n)
  (add1 (random n))) ;; STX random

; cyclic addition (e.g. for finding contrast values on color circle)
(define (cycadd a b base)
  (let ((factor
          (if (or (inee 0 1 a) (inee 0 1 b))
            (/ 1 (min a b))
            1)))
    (/
      (remainder
        (exact-round (* factor (+ a b))
        base))
      factor)))

; ((-> floor sqrt random) 10)
(define (-> . fs)
  (define (call-r fs x)
    (cond
      ((empty? fs) x)
      (else ((car fs) (call-r (cdr fs) x)))))
  (λ (x)
    (call-r fs x)))

; (->> floor sqrt random 10)
(define (->> . fs)
  (cond
    ((empty? (cdr fs)) (car fs))
    (else
      ((car fs) (apply ->> (cdr fs))))))

(define (clean f xs)
  (filter (λ (x) (not (f x))) xs))

(define (syntax->string stx)
  (let ((el (syntax->datum stx)))
    (cond
      ((list? el) #f)
      ((symbol? el) (symbol->string el))
      (else #f))))
