#lang racket

; alist - harrison in Hurrycap castle
; base - personal guard in Emerald city
; controls - harrison in Gyngeme cave
; hash - harrison in Blinkers country
; interval - post near Miners cave
; math - harrison in the Cannibal castle
; seqs - army camp near Emerald city
; strings - post near Marrans country
; type - harrison in Chewers country
; regexp - circulating unit
; time - post in the mountains towards Gyngeme rocks and outward regiment
; optimize - post near Magic Forest
; bytes - outpost near Stella country
; stats - outpost near Villina country
; json - Great River fleet
; tree - Siege crue
; symbols - Engineering crue
; checks - Orchestra crue
; hashtree - Information and propaganda crue
; http - Kamikadze crue

; graphics/tests: spies
; color_test - spies and police in the Emerald city and around
; svg_test - spy troop in the surrounding mountains
; layout_test - spy troop across the plain land

(module+ test

  (require rackunit)
  (require "../base.rkt")
  (require "../interval.rkt")

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
  (check-pred true? 0)
  (check-pred true? null)
  (check-equal? (true? #f) #f)

  (check-true (nonempty? '(2)))
  (check-true (not-empty? '(2)))
  (check-false (nonempty? '()))
  (check-false (not-empty? '()))
  (check-false (nonempty? 3))
  (check-false (not-empty? 3))

  (check-pred nil? null)
  (check-pred nil? empty)
  (check-pred nil? '())
  (check-pred nil? (hash))
  (check-pred nil? (list))
  (check-pred nil? (cdr '(a)))
  (check-pred nil? "")
  (check-pred nil? #f)
  (check-pred nil? (unless #t #t))

  (check-equal? (filter (and-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(11 17))

  (check-equal? (filter (or-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(1 3 11 12 14 17))

  (check-equal? (filter (and->
                          odd?
                          (or->
                            (λ (x) (< x 10))
                            (λ (x) (> x 100))
                         ))
                        '(1 2 3 11 12 14 17 23 118 121 123))
                '(1 3 121 123))

  (check-equal? (filter (and->
                          odd?
                          (not-> (or->
                            (λ (x) (< x 10))
                            (λ (x) (> x 100))
                         )))
                        '(1 2 3 11 12 14 17 23 118 121 123))
                '(11 17 23))

  (check-true (and* number? odd? 3))
  (check-false (and* number? odd? 4))
  (check-false (and* number? odd? "a"))

  (check-true (or* number? odd? 3))
  (check-true (or* number? odd? 4))
  (check-false (or* number? symbol? "a"))

  (check-pred notnil? #t)
  (check-pred notnil? "txt")
  (check-pred notnil? 3)
  (check-pred notnil? 3.141592)

  (check-pred znil? 0)
  (check-pred znil? null)
  (check-pred znil? #f)
  (check-pred znil? "")
  (check-pred znil? '())
  (check-pred znil? (hash))

  (check-equal? (!= 1 0) #t)
  (check-equal? (!= 1 1) #f)

  (check-equal? (map (rcurry / 5) '(10 5 1)) '(2 1 1/5))

  (check-equal? (clean odd? '(1 2 3 4 5)) '(2 4))
  ;
  (check-true (andmap (λ (x) (inii 1 10 x)) (for/list ((_ (range 100))) (rand 10))))

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

  (check-equal? (ok-or-false (first empty)) #f)
  (check-equal? (ok-or-false (second '(1))) #f)
  (check-equal? (ok-or-false (second '(1 2))) 2)
  (check-equal? (ok-or-false (third '(1 2 3 4 5))) 3)
  (check-equal? (ok-or-false (/ 1 0)) #f)
  (check-equal? (ok-or-false (sin 1 2)) #f)

  (check-equal? (symbol->list 'asdf) '(a s d f))
  (check-equal? (symbol->list 'a2) '(a |2|))
  (check-equal? (symbol->list 'a) '(a))
  (check-equal? (symbol->list 'foo) '(f o o))
  (check-equal? (symbol->list (string->symbol "")) '())

  (check-equal? (range+ 1 10) '(1 2 3 4 5 6 7 8 9 10))
  (check-equal? (range+ 3 3) '(3))
  (check-equal? (range+ 3 2) '())

  (check-equal? (dup 5 10) '(5 5 5 5 5 5 5 5 5 5))

  (check-equal? (but-last '(1 2 3)) '(1 2))
)
