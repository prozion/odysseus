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

(define (symbol-split sym delimeter)
  (map ->symbol
    (string-split (->string sym) delimeter)))

(module+ test

  (require rackunit)

  (check-equal? (rtrim-symbol 'a:) 'a)
  (check-equal? (rtrim-symbol 'a: 1) 'a)
  (check-equal? (rtrim-symbol 'abcd 2) 'ab)

  (check-equal? (ltrim-symbol '-a:) 'a:)
  (check-equal? (ltrim-symbol '-a: 1) 'a:)
  (check-equal? (ltrim-symbol 'abcd 2) 'cd)

  (check-equal? (symbol-split 'a.b.cd ".") '(a b cd))
  (check-equal? (symbol-split 'a.b.cd. ".") '(a b cd))
)
