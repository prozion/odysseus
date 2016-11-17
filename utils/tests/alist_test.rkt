#lang racket

(module+ test

  (require rackunit)
  (require "../alist.rkt")

  (check-pred alist? '((a 10) (b 20))) 
  (check-pred alist? '((a 10)))
  (check-pred alist? '(((1 2) 10)))
  (check-pred alist? '((null null)))
  (check-false (alist? 3))
  (check-false (alist? 'a))
  (check-false (alist? '(3)))
  (check-false (alist? '(3 4)))
  (check-false (alist? '((a 10) (b 2 3))))
)
