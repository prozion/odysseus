#lang racket

(require "../http.rkt")

(require rackunit)
(require "../checks.rkt")
(require "../list.rkt")
(require "../debug.rkt")

(define a 10)
(define b "hello")

(check-equal?
  (url-with-parameters "http://example.com" a b)
  "http://example.com?a=10&b=hello")
