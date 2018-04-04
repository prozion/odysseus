#lang racket

(module+ test

  (require rackunit)
  (require "../symbols.rkt")

  (check-equal? (rtrim-symbol 'a:) 'a)
  (check-equal? (rtrim-symbol 'a: 1) 'a)
  (check-equal? (rtrim-symbol 'abcd 2) 'ab)

  (check-equal? (ltrim-symbol '-a:) 'a:)
  (check-equal? (ltrim-symbol '-a: 1) 'a:)
  (check-equal? (ltrim-symbol 'abcd 2) 'cd)
)
