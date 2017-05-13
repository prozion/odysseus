#lang racket

(require "base.rkt")
(require "type.rkt")
(require "seqs.rkt")

(provide (all-defined-out))

;;;; basic data for functions

(define days '(31 28 31 30 31 30 31 31 30 31 30 31))
(define leap-days '(31 28 31 30 31 30 31 31 30 31 30 31))

(define (acc-days d)
  (for/fold
    ((s '(0)))
    ((i d))
    (pushr s (+ (last s) i))))

(define days-acc (acc-days days))
(define leap-days-acc (acc-days leap-days))

;;;; functions to work with time

(define (time->seconds astr)
  (let* ( (time-units (split astr ":"))
          (seconds (string->number (last time-units)))
          (minutes (string->number (if (null? (nth time-units -2)) "0" (nth time-units -2))))
          (hours (string->number (if (null? (nth time-units -3)) "0" (nth time-units -3)))))
    (+ (* 60 60 hours) (* 60 minutes) seconds)))

(define (seconds->days sec)
  (quotient sec (* 60 60 24)))

(define (seconds->hours sec)
  (quotient sec 3600))

(define (seconds->minutes sec)
  (quotient sec 60))

(define (seconds->time sec)
  (let* ((hours (seconds->hours sec))
        (minutes (- (seconds->minutes sec) (* 60 hours)))
        (seconds (- sec (* 3600 hours) (* 60 minutes)))
        (hours-print (~r #:min-width 2 #:pad-string "0" hours))
        (minutes-print (~r #:min-width 2 #:pad-string "0" minutes))
        (seconds-print (~r #:min-width 2 #:pad-string "0" seconds)))
    (cond
      ((> hours 0) (format "~a:~a:~a" hours-print minutes-print seconds-print))
      ((> minutes 0) (format "~a:~a" minutes-print seconds-print))
      (else (format "~a" seconds-print)))))

(define (time-diff t1 t2)
  (seconds->time
    (abs
      (- (time->seconds t2) (time->seconds t1)))))

(define (leap-year? year)
  (or
    (and
      (= 0 (remainder year 4))
      (!= 0 (remainder year 100)))
    (= 0 (remainder year 400))))

(define (date->days d)
  (let*
      ((date-lst (split d "."))
      ;(day (->number (first (split (first date-lst) "-")))) ; take first day in the days interval
      (day (->number (first date-lst)))
      (month (->number (second date-lst)))
      (year (->number (third date-lst)))
      (years (range 1 year))
      (leap-years (filter leap-year? years))
      (non-leap-years (clean leap-year? years)))
    (+
      (* 366 (length leap-years))
      (* 365 (length non-leap-years))
      ;; count days in months before:
      (nth
        (if (leap-year? year)
          leap-days-acc
          days-acc)
        month)
      ;; plus days in the current month:
      day)))

(define (date-diff d1 d2)
  (abs
    (- (date->days d2) (date->days d1))))

(define (comparison-f comp-op conversion)
  (λ (a b)
    (let* ((a (if (list? a) (car a) a))
          (b (if (list? b) (car b) b)))
      (comp-op (conversion a) (conversion b)))))

(define d> (comparison-f > date->days))
(define d< (comparison-f < date->days))
(define d= (comparison-f = date->days))

  ;(~r #:min-width 2 #:pad-string "0" 1)

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
