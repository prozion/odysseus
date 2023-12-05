#lang racket

(require rackunit)
(require "../vector.rkt")

(define test-vector (list->vector (range 1 10)))

(check-equal? (vector-ref* test-vector 0) 1)
(check-equal? (vector-ref* test-vector -1) 9)
(check-exn
  exn:fail?
  (λ ()
    (vector-ref* test-vector 11)))
(check-exn
  exn:fail?
  (λ ()
    (vector-ref* test-vector -15)))

(check-equal?
  (vector-conj (vector 1 2) (vector 10 11 12))
  (vector 1 2 (vector 10 11 12)))
(check-equal?
  (vector-conj (vector 1 2) 3 4 5)
  (vector 1 2 3 4 5))
(check-equal?
  (vector-conj (vector 1 2) 3 4 5 '(6 7))
  (vector 1 2 3 4 5 '(6 7)))
