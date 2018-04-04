#lang racket

(require "seqs.rkt")
(require "type.rkt")
(require "regexp.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define (rtrim-symbol sym (n 1))
  (->symbol (rtrim (->string sym) n)))

(define (ltrim-symbol sym (n 1))
  (->symbol (ltrim (->string sym) n)))
