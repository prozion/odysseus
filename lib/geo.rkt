#lang racket

(require "base.rkt")
(require "math.rkt")

(provide (all-defined-out))

; use Google Earth: hover to get heights, ruler to find horizontal distance
(define (get-angle bottom-height top-height horizontal-distance)
  (rad->grad (atan (/ (- top-height bottom-height) horizontal-distance))))

(define (angle->height ang hor-distance)
  (* hor-distance (tan (grad->rad ang))))

(define (make-lonlat-xy-transformation lon0 lat0 proj)
  (λ (lon lat)
    (case proj
      (('merc)
        (let* ((x (- lon lon0))
              ;(y (log (tan (+ (/ PI 4) (/ (- lat lat0) 2)))))) ; http://mathworld.wolfram.com/MercatorProjection.html
              (φ (- lat lat0))
              (y (log (+ (sec φ) (tan φ)))))
          (list x y)))
      (else (list lon lat)))))

;(define lonlat->xy (make-lonlat-xy-transformation lon0 lat0 'merc))
