#lang racket

(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (distance/speed->time distance speed)
  (seconds->time (/r distance (/ speed 3600))))
  ;(map (lambda (x) (x (/ (* 3600 (/ distance speed)) 60))) (list floor (lambda (x) (* 0.6 (- x (floor x)))))))

(define (speed->rate speed)
  (seconds->time (/r (* 60 60) speed)))

(define (distance/time->rate distance time)
  (seconds->time (/r (time->seconds time) distance)))

(define (distance/time->speed distance time)
  (/ distance 1000.0 (/ (time->seconds time) 3600)))
