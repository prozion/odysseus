#lang racket

(require "seqs.rkt")
(require "base.rkt")

(provide (all-defined-out))

(define (scalar? x)
  (or (number? x) (string? x) (symbol? x) (null? x)))

(define (simple-cons? x) 
  (and (pair? x) (not (list? x))))

(define (cons-ext? x)
  (and (pair? x) (scalar? (car x))))

(define (list2? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap list? x)))

(define (list-of-simple-cons? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap simple-cons? x)))

(define (list-of-seqs? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap pair? x)))

(define (type x)
  (cond
    ((number? x) 'number)
    ((string? x) 'string)
    ((bytes? x) 'bytes)
    ((alist? x) 'alist)
    ((list-of-simple-cons? x) 'list-of-cons)
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

; delta is for special case met in sorting newspapers issues:
; to compare say 160 and 160a, we add small number (0.1) to the latter, so 160a comes after 160
(define (->number x (delta 0))
  (cond
    ((number? x) x)
    ((false? x) 0)
    ((string? x)
      (let ((filtered (string-replace x (regexp ",") ".")))
        (cond
           ((string->number filtered) (string->number filtered))
           (else
            (let* (
                    (filtered (implode
                                        (filter
                                          (λ (x) (or (string->number x) (equal? x ".")))
                                          (explode filtered))))
                    (filtered (if (equal? filtered "") "0" filtered)))
              (+ delta (string->number filtered)))))))
    (else x)))

(define (->int x)
  (int (->number x)))

(define (->symbol x)
  (cond
    ((number? x) (string->symbol (number->string x)))
    ((string? x) (string->symbol x))
    (else x)))

(define (atom? x)
  (indexof? '(number string) (type x)))

(define (alist? lst)
  (define (list-of-2? lst)
    (and (list? lst) (= (length lst) 2)))
  (cond
    ((not (list? lst)) #f)
    ((null? (cdr lst)) (list-of-2? (car lst)))
    (else (and (list-of-2? (car lst)) (alist? (cdr lst))))))

(define (clist? seq)
  (andmap simple-cons? seq))
