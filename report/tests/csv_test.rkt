#lang racket

(module+ test

  (require rackunit)
  (require "../csv.rkt")

  (check-equal? (hash->csv-line (hash 'a 10 'b 20) '(b a))
                "20,10")

)
