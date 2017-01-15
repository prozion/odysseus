#lang racket

(require "base.rkt")
(require "math.rkt")

(provide (all-defined-out))

; use Google Earth: hover to get heights, ruler to find horizontal distance
(define (get-angle bottom-height top-height horizontal-distance)
  (rad->grad (atan (/ (- top-height bottom-height) horizontal-distance))))

(define (angle->height ang hor-distance)
  (* hor-distance (tan (grad->rad ang))))  
