#lang racket

(require "base.rkt")

(provide (all-defined-out))
;(provide (except-out (all-defined-out) >plain ))

(define (in a b x)
  (<= a x b))

(define inii in)

(define (inee a b x)
  (< a x b))

(define (inei a b x)
  (and (< a x) (<= x b)))

(define (inie a b x)
  (and (<= a x) (< x b)))

; cyclic addition (e.g. for finding contrast values on color circle)
(define (+c a b base)
  (let ((factor
          (if (or (inee 0 1 a) (inee 0 1 b))
            (/ 1 (min a b))
            1)))
    (/
      (remainder
        (exact-round (* factor (+ a b)))
        base)
      factor)))

; (>plain 10 8) -> #t
; (>plain 5 4) -> #t
(define (>plain x y)
  (let ((pris '(10 5 2 3 1)))
    (define (first-pri) (car pris))
    (define (next-pri n)
      (cadr (member n pris)))
    (define (>plain-n x y n)
      (let ((xrem (remainder x n))
            (yrem (remainder y n)))
        (cond
          ((= n 1) (< x y))
          ((and (= xrem 0) (= yrem 0)) (>plain (/ x n) (/ y n)))
          ((and (> xrem 0) (> yrem 0)) (>plain-n x y (next-pri n)))
          ((and (> xrem 0) (= yrem 0)) #f)
          ((and (= xrem 0) (> yrem 0)) #t)
          (else #t))))
    (>plain-n x y (first-pri))))

(define (fractize a b n #:crop (crop #t))
  (let ((res (fractize-2 a b n 0.3)))
    (if crop
      (take res n) ;; STX take ~list-ref
      res)))

; (fractize 1 16 3) -> '(5 10 15)
;(define (fractize-1 a b n)
;  (let* (
;          [step (/f (- b a) n)]
;          [plains (sort (range a (+ a step)) >plain)]
;          [start-a (car plains)])
;    (range start-a b step)))

(define (fractize-2 a b n p)
  (let* (
          [delta (*c (- b a) p)]
          [plains (sort (range a (+ a delta 1)) >plain)]
          [steps (filter
                    (λ (x) (and
                              (<= a (+ a (* x (dec n))) b)
                              (<= (- b delta) (+ a (* x n)) (+ b delta))))
                    (sort (range 1 (inc delta)) >plain))])
      (range (car plains) (+ b delta 1) (car steps))))

(define (find-first-digit x)
  (cond ((= x 0) 0)
        ((> 1 x 0) (find-first-digit (* x 10)))
        ((and (< x 10) (>= x 1)) (floor x))
        (else (find-first-digit (quotient x 10)))))

(define (find-number-order x)
  (cond
    ((= x 0) 0)
    (else (exact-floor (lg x 10)))))

(define (min-plain a)
  (let ([first-digit (number->string (find-first-digit a))]
        [order (number->string (find-number-order a))])
    (cond
      ((or (= a 1) (= a 0)) 0)
      (else (inexact->exact (string->number (string-append first-digit "e" order)))))))

(define (max-plain a)
  (let* ( [first-digit (inc (find-first-digit a))]
          [order (find-number-order a)])
            ;; STX let-values ~let
            (let-values (([first-digit order]
                            (if (= first-digit 10)
                              (values 1 (inc order))
                              (values first-digit order))))
                (inexact->exact (string->number (string-append (number->string first-digit) "e" (number->string order)))))))

; (fractize-3 1 16 1/4) -> '(0 5 10 15 20)
(define (fractize-3 a b step-ratio)
  (let* ( [start (min-plain a)]
          [end (max-plain b)]
          [step (inexact->exact (* (- end start) step-ratio))])
    (printf "~a ~a ~a~n~n" start end step)
    (range start (inc end) (exact->inexact step))))

(define (fractize-4 a b)
  (define (fractize-4-p a b p)
    (let* ( [start (min-plain a)]
            [end (max-plain b)]
            [step (car
                    (sort
                      (range 1 (*r (- end start) p))
                      >plain))])
      (range start end step)))
  (fractize-4-p a b 0.3))
