#lang racket

(require "alist.rkt")

(provide (all-defined-out))

(define (cons? x)
  (and (pair? x) (not (list? x))))

(define (list2? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap list? x)))

(define (list-of-cons? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap cons? x)))

(define (type? x)
  (cond
    ((number? x) 'number)
    ((string? x) 'string)
    ((bytes? x) 'bytes)
    ((alist? x) 'alist)
    ((list-of-cons? x) 'list-of-cons)
    ((list2? x) 'list2)
    ((list? x) 'list)
    ((pair? x) 'pair)
    ((char? x) 'char)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    ((syntax? x) 'syntax)
    ((vector? x) 'vector)
    ((hash? x) 'hash)
    ((path? x) 'path)
    (else #f)))

(define (->number x)
  (cond
    ((number? x) x)
    ((string? x) (string->number x))
    (else x)))
