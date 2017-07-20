#lang racket

(module+ test

  (require rackunit)
  (require "../layout.rkt")
  (require "../../lib/all.rkt")

  (check-= (get-proportion '(0 . 10) 50 100) 5 1e-10)
  (check-= (get-proportion '(10 . 20) 50 100) 15 1e-10)
  (check-= (get-proportion '(0 . 10) 120 100) 10 1e-10)
  (check-= (get-proportion '(0 . 10) 0 100) 0 1e-10)
  (check-= (get-proportion '(10 . 20) 0 100) 10 1e-10)
  (check-= (get-proportion '(0 . 10) 70 100) 7 1e-10)
  (check-= (get-proportion '(10 . 0) 70 100) 3 1e-10)

  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 0 0 110 110)))
  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 140 0 50 110)))
  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 140 140 50 50)))
  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 0 140 110 50)))
  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 100 100 50 50)))
  (check-true (bbox-overlap? (bbox 100 100 50 50) (bbox 110 110 10 10)))
  (check-false (bbox-overlap? (bbox 100 100 50 50) (bbox 160 160 50 50)))

  (check-true (segments-cross? (segment 6 4 11 14) (segment 5 9 14 2)))
  (check-true (segments-cross? (segment 5 9 14 2) (segment 13.99999 1.99999 16 4)))
  (check-false (segments-cross? (segment 5 9 14 2) (segment 12 6 18 8)))
  (check-false (segments-cross? (segment 6 4 11 14) (segment 12 6 18 8)))
  (check-false (segments-cross? (segment 5 9 14 2) (segment 4 10 2 12)))
  (check-false (segments-cross?
                (segment 525.9098300562504 663.8778525229247 533.9999999999999 658.0)
                (segment 1076.4894348370485 233.9098300562505 1076.4894348370485 263.9098300562505)))

  (check-true (bbox-overlap-segment? (bbox 100 100 100 100) (segment 120 0 120 300)))
  (check-true (bbox-overlap-segment? (bbox 100 100 100 100) (segment 0 0 300 300)))
  (check-false (bbox-overlap-segment? (bbox 100 100 100 100) (segment 250 250 300 300)))
  (check-false (bbox-overlap-segment? (bbox 920.4894348370485 233.9098300562505 156.0 30.0) (segment 525.9098300562504 663.8778525229247 533.9999999999999 658.0)))
  (check-true (bbox-overlap-segment? (bbox -83.60097345259452 -30.174393191407376 93.60000000000001 30.0) (segment 0.0 0 7.0710678118654755 -7.071067811865475)))

  (check-equal? (map int (segment->points (segment 34 89 110 300))) (list 34 89 110 300))
)
