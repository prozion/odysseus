#lang racket

(module+ test

  (require rackunit)
  (require "../base.rkt")
  (require "../interval.rkt")

  (check-equal? (% 5 3) 2)

  (check-equal? (int 23.0) 23)
  (check-equal? (int 23.9) 23)

  (check-equal? (dec 5) 4)

  (check-equal? (inc 5) 6)

  (check-equal? (// 5 3) 1.6666666666666667)

  (check-equal? (/r 5 3) 2)
  (check-equal? (/r 2 (sin 10)) -4)

  (check-equal? (*r 10 0.2 (sin 3) 80) 23)

  (check-equal? (*f (tan 1) (tan 2)) -4)
  (check-equal? (*f (tan 1) (tan -2)) 3)

  (check-equal? (*c (tan 1) (tan -2)) 4)

  (check-pred true? #t)
  (check-pred true? 0)
  (check-pred true? null)
  (check-equal? (true? #f) #f)

  (check-pred nil? null)
  (check-pred nil? empty)
  (check-pred nil? '())
  (check-pred nil? (hash))
  (check-pred nil? (list))
  (check-pred nil? (cdr '(a)))
  (check-pred nil? "")
  (check-pred nil? #f)
  (check-pred nil? (unless #t #t))

  (check-pred znil? 0)
  (check-pred znil? null)

  (check-equal? (!= 1 0) #t)
  (check-equal? (!= 1 1) #f)

  (check-equal? (map (rcurry / 5) '(10 5 1)) '(2 1 1/5))

  (check-equal? (clean odd? '(1 2 3 4 5)) '(2 4))
  ;
  (check-true (andmap (λ (x) (inii 1 10 x)) (for/list ((_ (range 100))) (rand 10))))

  (check-equal (filter (and-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(11 17))

  (check-equal (filter (or-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(1 3 11 12 14 17))
)
