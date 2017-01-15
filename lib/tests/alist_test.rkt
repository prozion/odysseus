#lang racket

(module+ test

  (require rackunit)
  (require "../alist.rkt")
  (require "../test.rkt")
  (require "../debug.rkt")

  (check-pred alist? '((a 10) (b 20)))
  (check-pred alist? '((a 10)))
  (check-pred alist? '(((1 2) 10)))
  (check-pred alist? '((null null)))
  (check-false (alist? 3))
  (check-false (alist? 'a))
  (check-false (alist? '(3)))
  (check-false (alist? '(3 4)))
  (check-false (alist? '((a 10) (b 2 3))))

  (check-equal? (firsts '((a 2) (b 10))) '(a b))

  (check-equal? (seconds '((a 2) (b 10))) '(2 10))

  (check-speed
              (alist-expand (dup-alist (1 2 3) 100) (range 1 1000) 50)
              10)
)
