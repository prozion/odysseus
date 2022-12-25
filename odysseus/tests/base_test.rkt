#lang racket

(require "../base.rkt")

(require rackunit)

(check-equal? (% 5 3) 2)

(check-equal? (int 23.0) 23)
(check-equal? (int 23.9) 23)

(check-= (fract 23.0) 0 1e-9)
(check-= (fract 23.9) 0.9 1e-9)

(check-equal? (dec 5) 4)

(check-equal? (inc 5) 6)

(check-equal? (// 5 3) 1.6666666666666667)
(check-= (// 5 3 1) 1.666666666666666 0.1)

(check-equal? (/r 5 3) 2)
(check-equal? (/r 5 3 1) 2)
(check-equal? (/r 2 (sin 10)) -4)

(check-equal? (/f 15 2 3) 2)

(check-equal? (/c 15 2 3) 3)

(check-equal? (*r 10 0.2 (sin 3) 80) 23)

(check-equal? (*f (tan 1) (tan 2)) -4)
(check-equal? (*f (tan 1) (tan -2)) 3)

(check-equal? (*c (tan 1) (tan -2)) 4)

(check-pred true? #t)
(check-pred true? (+ 2 2))
(check-pred true? (hash 'a 10))
(check-pred true? 0)
(check-pred true? null)
(check-equal? (true? #f) #f)

(check-true (not-empty? '(2)))
(check-false (not-empty? '()))
(check-false (not-empty? 5))

(check-pred nil? null)
(check-pred nil? empty)
(check-pred nil? '())
(check-pred nil? (hash))
(check-pred nil? (list))
(check-pred nil? (cdr '(a)))
(check-pred nil? "")
(check-pred nil? #f)
(check-pred nil? (unless #t #t))

(check-true (and* number? odd? 3))
(check-false (and* number? odd? 4))
(check-false (and* number? odd? "a"))

(check-true (or* number? odd? 3))
(check-true (or* number? odd? 4))
(check-false (or* number? symbol? "a"))

(check-pred not-nil? #t)
(check-pred not-nil? "txt")
(check-pred not-nil? 3)
(check-pred not-nil? 3.141592)

(check-equal? (!= 1 0) #t)
(check-equal? (!= 1 1) #f)

(check-equal? (clean odd? '(1 2 3 4 5)) '(2 4))

(check-equal? (ok-or-false (first empty)) #f)
(check-equal? (ok-or-false (second '(1))) #f)
(check-equal? (ok-or-false (second '(1 2))) 2)
(check-equal? (ok-or-false (third '(1 2 3 4 5))) 3)
(check-equal? (ok-or-false (/ 1 0)) #f)
(check-equal? (ok-or-false (sin 1 2)) #f)

(check-equal? (dup 5 10) '(5 5 5 5 5 5 5 5 5 5))

(check-equal? (butlast '(1 2 3)) '(1 2))
(check-equal? (butlast '(1)) '())
