#lang racket

(module+ test

  (require rackunit)
  (require "../strings.rkt")

  (check-equal? (dupstr "a" 5) "aaaaa")
  (check-equal? (dupstr "foo " 3) "foo foo foo ")

  ;(check-equal? (format-n "hello ~a" "world") "hello world")
  ;(check-equal? (format-n "hello ~l(, )" '("world" "verden")) "hello world, verden")

(check-equal? (strnumber->number "3") 3)
(check-= (strnumber->number "3,0") 3.0 1e-6)
(check-= (strnumber->number "2 100,50") 2100.5 1e-6)

(check-equal? (when/str (> 3 2) (format "~a ~a " "hello" 3) "world") "hello 3 world")
(check-equal? (when/str (< 3 2) (format "~a ~a " "hello" 3) "world") "")
)