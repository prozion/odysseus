#lang racket

(require "../controls.rkt")

(require rackunit)

(check-equal? (-> 100) 100)
(check-equal?
  (-> "foo-bar" (string-replace "-" "/") string-upcase (string-split "/"))
  '("FOO" "BAR"))

(check-equal? (gen 1 5) '(1 1 1 1 1))

(check-equal?
  (->
    "Lisp arriba!"
    (string-replace " " "-")
    (string-split "-")
    (->>
      (map string-length)
      (apply +))
    add1
    (+ 30))
  42)
