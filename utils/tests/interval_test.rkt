#lang racket

(module+ test

  (require rackunit)
  (require "../interval.rkt")

  (check-equal? (in 0 1 0.5) #t)
  (check-equal? (in 0 1 2) #f)
  (check-equal? (in 0 1 0) #t)
  (check-equal? (in 0 1 1) #t)

  (check-equal? (inii -100 100 -100) #t)

  (check-equal? (inee 1 3 2) #t)
  (check-equal? (inee 0 1 0) #f)
  (check-equal? (inee 0 1 1) #f)

  (check-equal? (inei 1 3 2) #t)
  (check-equal? (inei 0 1 0) #f)
  (check-equal? (inei 0 1 1) #t)

  (check-equal? (inie 1 3 2) #t)
  (check-equal? (inie 0 1 0) #t)
  (check-equal? (inie 0 1 1) #f)

  (check-equal? (+c 4 4 5) 3)
  (check-equal? (+c 3 2 8) 5)
  (check-equal? (+c 17 20 4) 1)

  (check-true (>plain 10 5))
  (check-true (>plain 4 3))
  (check-true (>plain 2 11))

  (check-equal? (fractize 1 16 3) '(5 10 15))
  (check-equal? (fractize 0 100 10) '(0 10 20 30 40 50 60 70 80 90))
  ;(check-equal? (fractize 0 100 4) '(0 30 60 90))
  
  (check-equal? (fractize-3 1 16 1/4) '(0 5 10 15 20))
)
