#lang racket

(module+ test

  (require rackunit)
  (require "../pd.rkt")
  (require "../../lib/checks.rkt")
  ;(require "../../lib/hash.rkt")

  (define rules
    (sbgn/pd
      (process
        (in (simple-chemical A))
          []
        (out (simple-chemical-B)))
      (process
        (in (simple-chemical B))
          []
        (out (simple-chemical-C)))))

  (check-hash-equal?
    (run-pd rules (hash 'A 1 'B 0 'C 0))
    (hash 'A 0 'B 0 'C 1))
  (check-hash-equal?
    (run-pd rules (hash 'A 1 'B 1 'C 0))
    (hash 'A 0 'B 0 'C 2))
  (check-hash-equal?
    (run-pd rules (hash 'A 0 'B 0 'C 0))
    (hash 'A 0 'B 0 'C 0))

)
