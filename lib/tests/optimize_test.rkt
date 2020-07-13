#lang racket

(module+ test

  (require rackunit)
  (require "../optimize.rkt")

  (check-equal? (opt/uniques '(8 1 2 3 3 4 5 2 10 2)) '(8 1 2 3 4 5 10))

  ;(check-equal? (opt/flatten '((8 1) ((2)) (3 (3 (4 5))) 2 10 2 '())) '(8 1 2 3 3 4 5 2 10 2))

  (check-equal? (opt/implode empty) "")
  (check-equal? (opt/implode '(" " "b" "a" "k" "e" "\n" "r" "y")) " bake\nry")
  (check-equal? (opt/implode '(1 2 3 4)) "1234")
  (check-equal? (opt/implode '(1 2 3 4) "+") "1+2+3+4")
  (check-equal? (opt/implode (list null null null) ",") ",,")

  (check-equal? (opt/split "a,b,c,,d,,,e" ",") '("a" "b" "c" "" "d" "" "" "e"))
  (check-equal? (opt/split "a,b,c,,d,,,e," ",") '("a" "b" "c" "" "d" "" "" "e" ""))
)
