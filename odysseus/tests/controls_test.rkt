#lang racket

(require "../controls.rkt")

(require rackunit)

(check-equal? (-> 100) 100)
(check-equal?
  (-> "foo-bar" (string-replace "-" "/") string-upcase (string-split "/"))
  '("FOO" "BAR"))

(check-equal? (gen 1 5) '(1 1 1 1 1))

(check-equal? (repeat-f append '(1 2 3) '((4) (5 (6)) (7))) '(1 2 3 4 5 (6) 7))
(check-equal? (repeat-f + 0 '(1 2 3)) 6)
(check-equal? (repeat-f + 0 '()) 0)
