#lang racket

(module+ test

  (require rackunit)
  (require "../strings.rkt")

  (check-equal? (dupstr "a" 5) "aaaaa")
  (check-equal? (dupstr "foo " 3) "foo foo foo ")

  ;(check-equal? (format-n "hello ~a" "world") "hello world")
  ;(check-equal? (format-n "hello ~l(, )" '("world" "verden")) "hello world, verden")
)
