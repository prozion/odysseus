#lang racket

(require "../lib/seqs.rkt")
(require "../lib/math.rkt")
(require "../lib/base.rkt")
(require "../lib/controls.rkt")
(require racket/format)

;(require dshirshov/utils)
;(require dshirshov/seqs)

;(provide (rgb->hsv rgb->hsl hsv->rgb hsl->rgb))
(provide (all-defined-out))

(define (rand-color)
  (let ((hex "0123456789abcdef"))
    (str "#" (implode (gen (nth hex (rand 16)) 6)))))

(define (str->rgb str)
  (map hex->dec (list (slice str 2 3) (slice str 4 5) (slice str 6 7))))

(define (rgb->str lst)
  (format "#~a~a~a"
          (~a (dec->hex (nth lst 1)) #:pad-string "0" #:width 2)
          (~a (dec->hex (nth lst 2)) #:pad-string "0" #:width 2)
          (~a (dec->hex (nth lst 3)) #:pad-string "0" #:width 2)))
  ;(format "#~X~X~X" (nth lst 1) (nth lst 2) (nth lst 3)))

; TODO: add contract with normalized values of r,g,b: (<= 0 x 1)
(define (hue col)
  (let* ( [r (nth col 1)] [g (nth col 2)] [b (nth col 3)]
          [r (// r 255)] [g (// g 255)] [b (// b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [d (- maxv minv)])
    (if (= maxv minv)
      0
      (//
        (cond
          ((= r maxv) (+ (// (- g b) d) (if (< g b) 6 0)))
          ((= g maxv) (+ (// (- b r) d) 2))
          ((= b maxv) (+ (// (- r g) d) 4)))
        6))))

(define (saturation1 col)
  (let* ( [r (nth col 1)] [g (nth col 2)] [b (nth col 3)]
          [r (/ r 255)] [g (/ g 255)] [b (/ b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [d (- maxv minv)])
    (if (= maxv 0)
      0
      (- 1 (// minv maxv)))))

(define (saturation2 col)
  (let* ( [r (nth col 1)] [g (nth col 2)] [b (nth col 3)]
          [r (/ r 255)] [g (/ g 255)] [b (/ b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [lumi (/ (+ maxv minv) 2)])
    (cond
      ((= 0 (+ maxv minv)) 0)
      ((< lumi 0.5)
        (// (- maxv minv) (+ maxv minv)))
      ((= 0 (- 2 maxv minv)) 0)
      (else
        (// (- maxv minv) (- 2 maxv minv))))))

(define (color-value col)
  (let* ([r (nth col 1)] [g (nth col 2)] [b (nth col 3)]
        [r (// r 255)] [g (// g 255)] [b (// b 255)])
    (max r g b)))

;(define (lightness r g b)
;  (let ( [r (/ r 255)] [g (/ g 255)] [b (/ b 255)])
;    (// (+ r g b) 3)))

; (rgb->hsv 250 10 120) -> '(1 0.5 0.9)
(define (rgb->hsv col)
  (let* (
        [h (hue col)]
        [s (saturation1 col)]
        [v (color-value col)])
    (list (*r 360 h) (*r 100 s) (*r 100 v))))

(define (rgb->hsl col)
  (let* ( [r (nth col 1)] [g (nth col 2)] [b (nth col 3)]
          [h (hue col)]
          [r (/ r 255)] [g (/ g 255)] [b (/ b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [l (// (+ maxv minv) 2)]
          [s
            (cond
              ((= 0 (+ maxv minv)) 0)
              ((< l 0.5)
                (// (- maxv minv) (+ maxv minv)))
              ((= 0 (- 2 maxv minv)) 0)
              (else
                (// (- maxv minv) (- 2 maxv minv))))])
        (list (*r 360 h) (*r 100 s) (*r 100 l))))

(define (hsv->rgb col)
  (match-let* ( [h (nth col 1)] [s (nth col 2)] [v (nth col 3)]
                [h (/ h 360)] [s (/ s 100)] [v (/ v 100)]
                [i (floor (* h 6))]
                [f (- (* h 6) i)]
                [p (* v (- 1 s))]
                [q (* v (- 1 (* f s)))]
                [t (* v (- 1 (* (- 1 f) s)))]
                [(list r g b)
                  (case (% i 6)
                    ((0) (list v t p))
                    ((1) (list q v p))
                    ((2) (list p v t))
                    ((3) (list p q v))
                    ((4) (list t p v))
                    ((5) (list v p q)))])
    (map (curry *r 255) (list r g b))))

; http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
(define (hsl->rgb col)
  (define (->01 x)
    (cond
      ((> x 1) (- x 1))
      ((< x 0) (+ x 1))
      (else x)))
  (define (taux tc t1 t2)
    (let (  [c1 (* 6 tc)]
            [c2 (* 2 tc)]
            [c3 (* 3 tc)])
      (cond
        ((< c1 1) (+ t2 (* 6 tc (- t1 t2))))
        ((< c2 1) t1)
        ((< c3 2) (+ t2 (* 6 (- 2/3 tc) (- t1 t2))))
        (else t2))))
  (let* ( [h (nth col 1)] [s (nth col 2)] [l (nth col 3)]
          [h (// h 360)] [s (// s 100)] [l (// l 100)])
    (if (= s 0)
      (gen (*r 255 l) 3)
      (let* ( [t1 (if (< l 0.5)
                    (* l (+ 1 s))
                    (+ l s (* -1 l s)))]
              [t2 (- (* 2 l) t1)]
              [tr (->01 (+ h 1/3))]
              [tg h]
              [tb (->01 (- h 1/3))]
              [r (taux tr t1 t2)]
              [g (taux tg t1 t2)]
              [b (taux tb t1 t2)])
          (map (curry *r 255) (list r g b))))))

(define (string-rgb->model f)
  (λ (str)
    (define (cnvrt str i1 i2)
      (hex->dec (slice str i1 i2)))
    (f (list (cnvrt str 2 3) (cnvrt str 4 5) (cnvrt str 6 7)))))

(define string-rgb->hsl (string-rgb->model rgb->hsl))
(define string-rgb->hsv (string-rgb->model rgb->hsv))

(define (color+ base delta (limit 100))
  (let*
      ( (ncolor (+ base delta))
        (ncolor (if (> ncolor limit) limit ncolor))
        (ncolor (if (< ncolor 0) 0 ncolor)))
    ncolor))

; make color darker, -degree makes lighter
; (shadow #fe3578 10)
(define (color/shadow color degree)
  (let*
      (
        (rgb-color (str->rgb color))
        (hsv-color (rgb->hsv rgb-color))
        (ncolor (color+ (nth hsv-color 3) degree))
        (modified-hsv (setn hsv-color 3 ncolor))
        (modified-rgb (hsv->rgb modified-hsv))
        (res (rgb->str modified-rgb))
      )
    res))

(define (color/zebra lst delta)
  (define (color/zebra/iter lst res curdelta)
    (cond
      ((null? lst) res)
      (else (color/zebra/iter (cdr lst) (rpush res (color/shadow (car lst) curdelta)) (- curdelta)))))
  (color/zebra/iter lst empty delta))

(define (grade-color base-color grade total-grades #:delta-v (delta-v -10) #:delta-s (delta-s -40))
  (let*
      ( (clr (if (string? base-color) (str->rgb base-color) base-color))
        (clr (rgb->hsv clr))
        (h (nth clr 1))
        (s (nth clr 2))
        (v (nth clr 3))
        ;(delta-v (/f (- 100 v) 2))
        (step (/f 360 (/ total-grades 3)))
        (rmnd (remainder grade 3))
        (res
          (rgb->str
            (hsv->rgb
              (list
                (+ h (* step (quotient grade 3)))
                ;(+ h (* step (quotient grade 3)))
                (if (= 2 rmnd)
                  (+ s delta-s)
                  s)
                (cond
                  ((= 1 rmnd) (color+ v delta-v))
                  ((= 2 rmnd) (color+ v (/ delta-v 2)))
                  (else v)))))))
      ;(println res)
      res))
