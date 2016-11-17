#lang racket

(module+ test

  (require rackunit)
  (require "../base.rkt")

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

  (check-pred nil? null)
  (check-pred nil? empty)
  (check-pred nil? '())
  (check-pred nil? (list))
  (check-pred nil? (cdr '(a)))
  (check-pred nil? "")

  (check-equal? (!= 1 0) #t)
  (check-equal? (!= 1 1) #f)

  (check-equal? (map (rcurry / 5) '(10 5 1)) '(2 1 1/5))

  (check-equal? (clean odd? '(1 2 3 4 5)) '(2 4))
  ;
  (check-true (andmap (λ (x) (inii 1 10 x)) (for/list ((_ (range 100))) (rand 10)))) 
  ;
  (check-equal? (+c 4 4 5) 3)
  (check-equal? (+c 3 2 8) 5)
  (check-equal? (+c 17 20 4) 1)
)
