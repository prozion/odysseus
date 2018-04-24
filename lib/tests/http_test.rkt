#lang racket

(module+ test

  (require rackunit)
  (require "../checks.rkt")
  (require "../http.rkt")
  (require "../seqs.rkt")
  (require "../debug.rkt")

  (define a 10)
  (define b "hello")

  (check-equal?
    (url-with-parameters "http://example.com" a b)
    "http://example.com?a=10&b=hello")
)
