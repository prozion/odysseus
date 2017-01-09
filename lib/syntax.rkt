#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

;(define (syntax->string stx)
;  (let ((el (syntax->datum stx)))
;    (cond
;      ((list? el) #f)
;      ((symbol? el) (symbol->string el))
;      (else #f))))

(define (odd-f f lst)
  (cond
    ((null? lst) null)
    ((null? (cadr lst)) (cdr lst))
    (else (cons (f (car lst)) (cons (cadr lst) (odd-f f (cddr lst)))))))
