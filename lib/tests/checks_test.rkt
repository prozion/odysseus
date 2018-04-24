#lang racket

(module+ test

  (require rackunit)
  (require "../checks.rkt")

  (check-true
    (check-hash
      (hash)
      (hash)))

  (check-true
    (check-hash
      (hash 'a 10 'b 20)
      (hash 'b 20 'a 10)))

  (check-false
    (check-hash
      (hash 'a 10 'b 30)
      (hash 'b 20 'a 10)))

  (check-true
    (check-hash
      (hash '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")
      (hash '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")))

  (check-true
    (check-hash
      (hash '(2 (3 1 10)) '(3 4))
      (hash '((10 1 3) 2) '(4 3))
      #t))

  (check-true
    (check-hash
      (hash '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")
      (hash '(((#f enzyme) (#f S1 (simple chemical))) (#f P1 (simple chemical))) "positive influence")
      #t))
)
