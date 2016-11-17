#lang racket

(require (prefix-in base: "base.rkt")) ; STX prefix-in

(provide (all-defined-out))

(define base-type? type?)

; expand type?
(define (type? x)
  (cond
    ((alist? x) 'alist)
    ((base:type? x) (base:type? x))
    (else #f)))

(define (alist? lst)
  (define (list-of-2? lst)
    (and (list? lst) (= (length lst) 2)))
  (cond
    ((not (list? lst)) #f)
    ((null? (cdr lst)) (list-of-2? (car lst)))
    (else (and (list-of-2? (car lst)) (alist? (cdr lst))))))
