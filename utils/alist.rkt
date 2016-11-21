#lang racket

(require (prefix-in base: "base.rkt")) ; STX prefix-in

(provide (all-defined-out))

(define (alist? lst)
  (define (list-of-2? lst)
    (and (list? lst) (= (length lst) 2)))
  (cond
    ((not (list? lst)) #f)
    ((null? (cdr lst)) (list-of-2? (car lst)))
    (else (and (list-of-2? (car lst)) (alist? (cdr lst))))))

; (firsts '((a 2) (b 10))) -> '(a b)
(define (firsts alst)
  (cond
    ((null? alst) null)
    (else (cons (first (car alst)) (firsts (cdr alst))))))

; (seconds '((a 2) (b 10))) -> '(2 10)
(define (seconds alst)
  (cond
    ((null? alst) null)
    (else (cons (second (car alst)) (seconds (cdr alst))))))
