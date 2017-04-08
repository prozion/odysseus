#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

;; geometric functions

(define (distance x1 y1 x2 y2)
  (sqrt (+
          (sqr (- x2 x1))
          (sqr (- y2 y1)))))

(define (get-angle x y)
  (cond
    ((= x 0) (if (>= y 0) (grad->rad 90) (grad->rad 270)))
    ((and (>= x 0) (>= y 0)) (atan (/ y x)))
    ((and (< x 0) (>= y 0))  (+ (grad->rad 180) (atan (/ y x))))
    ((and (< x 0) (< y 0))   (+ (grad->rad 180) (atan (/ y x))))
    ((and (> x 0) (< y 0))   (+ (grad->rad 360) (atan (/ y x))))
    (else (atan (/ y x)))))

(define (angle-quadrant alpha)
  (cond
    ((< alpha 0) (angle-quadrant (abs alpha)))
    ((>= (rad->grad alpha) 360) (angle-quadrant (grad->rad (remainder (round (rad->grad alpha)) 360))))
    ((<= 0 alpha (grad->rad 90)) 1)
    ((<= (grad->rad 90) alpha (grad->rad 180)) 2)
    ((<= (grad->rad 180) alpha (grad->rad 270)) 3)
    ((<= (grad->rad 270) alpha (grad->rad 360)) 4)))

;; general functions for layout control in svg

(define (svg/translate x y)
  (str "translate(" x " " y ")"))

(define (svg/rotate ang x y)
  (format "rotate(~a ~a ~a)" ang x y))
