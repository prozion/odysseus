#lang racket

(require (rename-in racket/base (length list-length)))

(provide (all-defined-out))

(define (mutable-vector v)
  (apply vector (vector->list v)))

(define (get v i)
  (vector-ref v i))

(define (length v)
  (cond
    ((list? v) (list-length v))
    (vector? (vector-length v))
    (else (list-length v))))

(define (-- x)
  (- x 1))

(define (++ x)
  (+ x 1))

(define (set v i val)
  (let ((v-mutable (mutable-vector v)))
    (vector-set! v-mutable i val)
    v-mutable))
