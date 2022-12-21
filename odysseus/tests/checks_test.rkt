#lang racket

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
    #:list-any-order? #t))

(check-true
  (check-hash
    (hash '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")
    (hash '(((#f enzyme) (#f S1 (simple chemical))) (#f P1 (simple chemical))) "positive influence")
    #:list-any-order? #t))

(check-false
  (check-hash
    (hash '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")
    (hash '(((#f enzyme) (#f S1 (simple chemical))) (#f P1 (simple chemical))) "positive influence")
    #:list-any-order? #f))

(check-false
  (check-hash
    (hash
      '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence"
      '(((P1 P2) (A1 A2 A3)) (#f P1 (simple chemical))) "negative influence")
    (hash
      '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence"
      '(((P1 P2) (A1 A2 A3)) (#f P1 (simple chemical))) "positive influence")
    #:values-any-order? #t))

(check-true
  (check-hash
    (hash
      '(((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence"
      '(((P1 P2) (A1 A2 A3)) (#f P1 (simple chemical))) "negative influence")
    (hash
      '((#f P1 (simple chemical)) ((S1 (simple chemical) #f) (#f enzyme))) "positive influence"
      '((#f P1 (simple chemical)) ((P1 P2) (A3 A1 A2))) "negative influence")
    #:values-any-order? #t))
