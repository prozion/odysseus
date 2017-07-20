#lang racket

(require "base.rkt")
(require "math.rkt")

(provide (all-defined-out))

; use Google Earth: hover to get heights, ruler to find horizontal distance
(define (height->angle bottom-height top-height horizontal-distance)
  (rad->grad (atan (/ (- top-height bottom-height) horizontal-distance))))

(define (angle->height ang hor-distance)
  (* hor-distance (tan (grad->rad ang))))

(define (make-lonlat-xy-transformation #:lon0 lon0 #:lat0 lat0 #:proj (proj 'merc) #:kx (kx 30) #:ky (ky -0.001) #:dx (dx 0) #:dy (dy 0) #:+a (+a 6378137) #:+b (+b 6378137))
  (λ (pair)
    (let* ((lon (car pair))
          (lon (if (< lon 0) (+ 360 lon) lon))
          (lat (cdr pair)))
    (case proj
      ((merc)
        (let* ((x (+ dx (* kx (- lon lon0))))
              ;(y (log (tan (+ (/ PI 4) (/ (- lat lat0) 2)))))) ; http://mathworld.wolfram.com/MercatorProjection.html
              (φ (grad->rad (- lat lat0)))
              ;(y (log (+ (sec φ) (tan φ)))))
              (ec (sqrt (- 1 (sqr (/ +b +a)))))
              (y
                (+ dy (* +a ky (log
                        (*
                          (tan (+ (/ PI 4) (/ φ 2)))
                          1))))))
                          ;(expt (/ (- 1 (* ec (sin φ))) (+ 1 (* ec (sin φ)))) (/ ec 2)))))))
          (cons x y)))
      (else (cons lon lat))))))

(define (hms->number h m s #:hemisphere (hemisphere 'east))
  (define (hms->seconds h m s #:hemisphere (hemisphere 'east))
    (if (equal? hemisphere 'west)
      (- (hms->seconds 360 0 0) (hms->seconds h m s))
      (+ s (* 60 m) (* 60 60 h))))
    (/ (hms->seconds h m s #:hemisphere hemisphere) 3600.0))
