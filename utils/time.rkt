#lang racket

(provide (all-defined-out))

;;;; functions to work with time

;; add hours and minutes
(define (add-hourly . hs)
  (define (rst a) (- a (floor a)))
    (let* ( [intsum (apply + (map floor hs))]
            [fractsum (apply + (map rst hs))]
            [h_fractsum (floor (/ fractsum 0.6))]
            [m (- fractsum (* 0.6 h_fractsum))])
  ;(printf "h1=~a, prod=~a, h2=~a, m=~a\n" h1 prod h2 m)
      (/
        (round
          (*
            100
            (+ intsum h_fractsum m)))
        100)))
