#lang racket

(require (prefix-in base: "base.rkt"))
(require "seqs.rkt")
(require "strings.rkt")
(require "debug.rkt")
(require compatibility/defmacro)
(require (for-syntax racket/list))

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

(define-macro (dup-alist lst n)
    `(map (λ (x) (quote ,lst)) (range 1 ,n)))

; (alist-expand '((1 2) (3 4)) '(10 20) 1) -> '((1 2 10) (1 2 20) (3 4))
(define (alist-expand base-list expander-list pos)
  (let ((pos-el (nth base-list pos)))
      (splice
        (remove base-list pos)
        (map
          (λ (x) (pushr pos-el x))
          expander-list)
        pos)))

(define (alist-flatten alst)
  (foldl merge '() alst))

(define (alist->mstring alst (frmt (dupstr "~a " (length (car alst)))))
  (implode
    (map
      (λ (x) (apply (curry format frmt) x))
      alst)
    "\n"))
