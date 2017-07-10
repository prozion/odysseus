#lang racket

(require "base.rkt")
(require "math.rkt")

(provide (all-defined-out))

; use Google Earth: hover to get heights, ruler to find horizontal distance
(define (height->angle bottom-height top-height horizontal-distance)
  (rad->grad (atan (/ (- top-height bottom-height) horizontal-distance))))

(define (angle->height ang hor-distance)
  (* hor-distance (tan (grad->rad ang))))

(define (make-lonlat-xy-transformation lon0 lat0 proj (scale-x 30) (scale-y -0.001) (+a 6378137) (+b 6378137))
  (λ (lon lat)
    (case proj
      ((merc)
        (let* ((x (* scale-x (- lon lon0)))
              ;(y (log (tan (+ (/ PI 4) (/ (- lat lat0) 2)))))) ; http://mathworld.wolfram.com/MercatorProjection.html
              (φ (grad->rad (- lat lat0)))
              ;(y (log (+ (sec φ) (tan φ)))))
              (ec (sqrt (- 1 (sqr (/ +b +a)))))
              (y
                (* +a scale-y (log
                        (*
                          (tan (+ (/ PI 4) (/ φ 2)))
                          1)))))
                          ;(expt (/ (- 1 (* ec (sin φ))) (+ 1 (* ec (sin φ)))) (/ ec 2)))))))
          (cons x y)))
      (else (cons lon lat)))))

;(define lonlat->xy (make-lonlat-xy-transformation lon0 lat0 'merc))
