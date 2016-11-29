#lang racket

(require "../utils/seqs.rkt")
(require "../utils/base.rkt")
(require "../utils/controls.rkt")

;(require dshirshov/utils)
;(require dshirshov/seqs)

;(provide (rgb->hsv rgb->hsl hsv->rgb hsl->rgb))
(provide (all-defined-out))

(define (rand-color)
  (let ((hex "0123456789abcdef"))
    (str "#" (implode (gen (nth hex (rand 16)) 6)))))

; "a2" -> 162
(define (hex->dec v)
  (string->number (str "#x" v)))

; 162 -> "a2"
(define  (dec->hex v)
  (define (next-hex v)
    (let ((hex "0123456789abcdef"))
      (nth hex (add1 (% v 16)))))
  (define (dec->hex-r v)
    (cond
      ((= v 0) "")
      (else (str (dec->hex-r (quotient v 16)) (next-hex (% v 16)) ))))
  (cond
    ((= v 0) "0")
    (else (dec->hex-r v))))

(define (str->rgb str)
  (map hex->dec (list (slice str 1 2) (slice str 3 4) (slice str 5 6))))

(define (rgb->str lst)
  (format "#~X~X~X" (nth lst 1) (nth lst 2) (nth lst 3)))

; TODO: add contract with normalized values of r,g,b: (<= 0 x 1)
(define (hue r g b)
  (let* ( [r (// r 255)] [g (// g 255)] [b (// b 255)]
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

(define (saturation1 r g b)
  (let* ( [r (/ r 255)] [g (/ g 255)] [b (/ b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [d (- maxv minv)])
    (if (= maxv 0)
      0
      (- 1 (// minv maxv)))))

(define (saturation2 r g b)
  (let* ( [r (/ r 255)] [g (/ g 255)] [b (/ b 255)]
          [maxv (max r g b)] [minv (min r g b)]
          [lumi (/ (+ maxv minv) 2)])
    (cond
      ((= 0 (+ maxv minv)) 0)
      ((< lumi 0.5)
        (// (- maxv minv) (+ maxv minv)))
      ((= 0 (- 2 maxv minv)) 0)
      (else
        (// (- maxv minv) (- 2 maxv minv))))))

(define (color-value r g b)
  (let ([r (// r 255)] [g (// g 255)] [b (// b 255)])
    (max r g b)))

;(define (lightness r g b)
;  (let ( [r (/ r 255)] [g (/ g 255)] [b (/ b 255)])
;    (// (+ r g b) 3)))

; (rgb->hsv 250 10 120) -> '(1 0.5 0.9)
(define (rgb->hsv r g b)
  (let ([h (hue r g b)]
        [s (saturation1 r g b)]
        [v (color-value r g b)])
    (list (*r 360 h) (*r 100 s) (*r 100 v))))

(define (rgb->hsl r g b)
  (let* ( [h (hue r g b)]
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

(define (hsv->rgb h s v)
  (match-let* ( [h (/ h 360)] [s (/ s 100)] [v (/ v 100)]
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
(define (hsl->rgb h s l)
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
  (let ( [h (// h 360)] [s (// s 100)] [l (// l 100)])
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

;(define (hsl->rgb h s l)
;  (define (hue->rgb p q t)
;  	(cond
;    	((< t 0) (+ t 1))
;    	((> t 1) (- t 1))
;   	 	((< t (/ 1 6)) (+ p (* (- q p) 6 t)))
;    	((< t 0.5) q)
;    	((< t (/ 2 3)) (+ p (* (- q p) 6 (- (/ 2 3) t))))
;    	(else p)))
;  (let ( [h (/ h 360)] [s (/ s 100)] [l (/ l 100)])
;    (if (= s 0)
;      (gen (*r 255 l) 3)
;      (let* (
;              [q (if (< l 0.5)
;                    (* l (+ 1 s))
;                    (+ l (- s (* l s))))]
;              [p (- (* 2 l) q)]
;              [r (hue->rgb p q (+ h (/ 1 3)))]
;              [g (hue->rgb p q h)]
;              [b (hue->rgb p q (- h (/ 1 3)))])
;        (map (curry *f 255) (list r g b))))))

(define (string-rgb->model f)
  (λ (str)
    (define (cnvrt str i1 i2)
      (hex->dec (slice str i1 i2)))
    (f (cnvrt str 2 3) (cnvrt str 4 5) (cnvrt str 6 7))))

(define string-rgb->hsl (string-rgb->model rgb->hsl))
(define string-rgb->hsv (string-rgb->model rgb->hsv))
