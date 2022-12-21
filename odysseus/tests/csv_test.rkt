#lang racket

(require "../csv.rkt")
(require rackunit)

(check-equal? (get-csv '(b a) (list (hash 'a 10 'b 20)))
              "b,a\n20,10")
