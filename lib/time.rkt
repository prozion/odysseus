#lang racket

(require "seqs.rkt")

(provide (all-defined-out))

;;;; functions to work with time

;; add hours and minutes
(define (+h . hs)
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

(define (timeline . body)
  #t
  ; take arguments as triplets
)

; 2017-01-19T18:00:00 -> (hash 'year "2017" 'month "01" 'day "19" 'hour "18" 'min "00" 'sec "00")
(define (parse-time timestr)
  (let* ((ts (split timestr "T"))
        (t1s (split (nth ts 1) "-"))
        (t2s (split (nth ts 2) ":"))
        (year (nth t1s 1))
        (month (nth t1s 2))
        (day (nth t1s 3))
        (hour (nth t2s 1))
        (minute (nth t2s 2))
        (sec (nth t2s 3)))
    (hash 'year year 'month month 'day day 'hour hour 'min minute 'sec sec)))

(define (th->string th (templatestr ""))
  (format "~a.~a ~a:~a" (hash-ref th 'day) (hash-ref th 'month) (hash-ref th 'hour) (hash-ref th 'min)))

(define (time-reformat timestr)
  (th->string
    (parse-time timestr)))
