#lang racket

(require compatibility/defmacro)
(require "../lib/load/all.rkt")
(require "geometry.rkt")

(provide (all-defined-out))

;; geometric functions

(define (get-proportion interval value max-value)
  (let* ((inverse (> (car interval) (cdr interval)))
        (interval (if inverse
                    (cons (cdr interval) (car interval))
                    interval))
        (tres (cond
                ((> value max-value) (cdr interval))
                ((< value 0) (car interval))
                (else
                  (+ (car interval) (* 1.0 (/ value max-value) (- (cdr interval) (car interval)))))))
        (res (if inverse
                (+ (car interval) (- (cdr interval) tres))
                tres)))
    res))

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

(define (bbox x y w h)
  (list x y w h))

(define (bbox-accessor i)
  (λ (bbox) (nth bbox i)))

(define bbox-x (bbox-accessor 1))
(define bbox-y (bbox-accessor 2))
(define bbox-w (bbox-accessor 3))
(define bbox-h (bbox-accessor 4))

(define (bbox-overlap? a b)
  (let* ((ax (bbox-x a)) (bx (bbox-x b))
        (ay (bbox-y a)) (by (bbox-y b))
        (aw (bbox-w a)) (bw (bbox-w b))
        (ah (bbox-h a)) (bh (bbox-h b))
        (ax1 ax) (ay1 ay)  (ax2 (+ ax aw)) (ay2 (+ ay ah))
        (bx1 bx) (by1 by)  (bx2 (+ bx bw)) (by2 (+ by bh)))
    (or
      (and (<= ax1 bx1 ax2) (<= ay1 by1 ay2)) ; nw inside
      (and (<= ax1 bx2 ax2) (<= ay1 by1 ay2)) ; ne inside
      (and (<= ax1 bx1 ax2) (<= ay1 by2 ay2)) ; sw inside
      (and (<= ax1 bx2 ax2) (<= ay1 by2 ay2)) ; se inside
      (and (<= ax1 bx2 ax2) (<= ay1 by2 ay2)) ; se inside
      ;; check another rectangle
      (and (<= bx1 ax1 bx2) (<= by1 ay1 by2)) ; nw inside
      (and (<= bx1 ax2 bx2) (<= by1 ay1 by2)) ; ne inside
      (and (<= bx1 ax1 bx2) (<= by1 ay2 by2)) ; sw inside
      (and (<= bx1 ax2 bx2) (<= by1 ay2 by2)) ; se inside
      (and (<= bx1 ax2 bx2) (<= by1 ay2 by2)) ; se inside
 )))

; finds free spot for label
(define (next-free-place bboxes x y w h #:segments (segments #f) #:step0 (step0 10) #:step (step 3) #:debug (debug #f))
  (let ((φs (map grad->rad (range -45 325 3))))
    (define (next-free-place-iter cnt curphi curstep)
      (let* (
            (φ (nth-cycled φs curphi))
            (dx (* curstep (cos φ)))
            (dy (* curstep (sin φ)))
            ;(_ (printf "~a ~a~n" dx dy))
            (bcur (cond
                    ((> dx 0) (bbox (+ x dx) (+ y dy) w h))
                    ((< dx 0) (bbox (- (+ x dx) w) (+ y dy) w h))
                    ((= dx 0) (bbox (- (+ x dx) (/ w 2.0)) (+ y dy) w h))))
            (lcur (segment x y (+ x dx) (+ y dy))))
        ;(when debug (printf "~a ~a~n" debug φ))
        (cond
          ((> cnt (length φs)) (next-free-place-iter 1 (inc (random (length φs))) (+ curstep step)))
          ((not
              (or
                (ormap
                  (λ (b) (bbox-overlap? b bcur))
                  bboxes)
           ))
            (hash 'dx dx 'dy dy 'bbox bcur))
          (else
            ;(printf "~a~n   ~a~n      ~a~n         ~a~n            ~a~n~n"
            ;        bcur
            ;        bboxes
            ;        (map
            ;          (λ (b) (bbox-overlap? b bcur))
            ;          bboxes)
            ;        (map
            ;          (λ (l) (bbox-overlap-segment? bcur l))
            ;          segments)
            ;        (map
            ;          (λ (b) (bbox-overlap-segment? b lcur))
            ;          bboxes))
            (next-free-place-iter (inc cnt) (inc curphi) curstep)))))
    (next-free-place-iter 1 1 step0)))

(define (segment x1 y1 x2 y2) ; finds start and end t-parameter for parametric form of segment'(x1 y1 x2 y2) -> hash(:a :b :y0 :t1 :t2)
  (cond
    ((= x1 x2) (hash 'k #f 'y0 x1 'tstart y1 'tend y2))
    ((= y1 y2) (hash 'k 0 'y0 y1 'tstart x1 'tend x2))
    (else
      (let* ((k (/ (- y2 y1 0.0) (- x2 x1)))
            (y0 (- y1 (* 1.0 k x1)))
            (t1 (/ (- y1 y0) k))
            (t2 (/ (- y2 y0) k))
            (tstart (if (> t2 t1) t1 t2))
            (tend (if (> t2 t1) t2 t1)))
        (hash 'k k 'y0 y0 'tstart tstart 'tend tend)))))

(define (segment->points seg)
  (let* ((k (hash-ref seg 'k))
        (y0 (hash-ref seg 'y0))
        (tstart (hash-ref seg 'tstart))
        (tend (hash-ref seg 'tend))
        (y1 (if k
              (+ (* tstart k 1.0) y0)
              tstart))
        (y2 (if k
              (+ (* tend k 1.0) y0)
              tend))
        (x1 (if k
              (if (!= 0 k)
                (/ (- y1 y0 0.0) k)
                tstart)
              y0))
        (x2 (if k
              (if (!= 0 k)
                (/ (- y2 y0 0.0) k)
                tend)
              y0)))
    (list x1 y1 x2 y2)))

(define (segments-cross? s1 s2) ; finds t at which s1 crosses with s2
  (define (is-between a x b)
    (or
      (<= a x b)
      (<= b x a)))
  (let* ((k1 (hash-ref s1 'k))
        (k2 (hash-ref s2 'k))
        (a1 (hash-ref s1 'y0))
        (a2 (hash-ref s2 'y0))
        (tstart1 (hash-ref s1 'tstart))
        (tend1 (hash-ref s1 'tend))
        (tstart2 (hash-ref s2 'tstart))
        (tend2 (hash-ref s2 'tend))
     )
    (cond
      ((and (not k1) (not k2)) (or (< tstart1 tstart2 tend1) (< tstart1 tend2 tend1)))
      ;((not k1)
      ;  (let* ((yc (+ (* k2 a1) a2))
      ;        (tc2 (/ (- yc a2) k2)))
      ;    (and
      ;      (< tstart2 tc2 tend2)
      ;      (< tstart1 yc tend1))))
      ;((not k2)
      ;  (let* ((yc (+ (* k1 a2) a1))
      ;        (tc1 (/ (- yc a1) k1)))
      ;    (and
      ;      (< tstart1 tc1 tend1)
      ;      (< tstart2 yc tend2))))
      ((and k1 k2 (= k1 k2) (= a1 a2)) #t) ; coincide
      ((and k1 k2 (= k1 k2) (!= a1 a2)) #f) ; parallel
      (else
        (let* ((xc (cond
                      ((not k1) a1)
                      ((not k2) a2)
                      (else
                        (/ (- a2 a1) (- k1 k2)))))
              (yc (cond
                      ((not k1) (+ a2 (* k2 xc)))
                      ((not k2) (+ a1 (* k1 xc)))
                      (else (+ a1 (* k1 xc)))))
              (tc1 (cond
                      ((not k1) yc)
                      ((= k1 0) xc)
                      (else
                        (/ (- yc a1) k1))))
              (tc2 (cond
                      ((not k2) yc)
                      ((= k2 0) xc)
                      (else
                        (/ (- yc a2) k2)))))
          (and
            (is-between tstart1 tc1 tend1)
            (is-between tstart2 tc2 tend2)))))))

(define (bbox-overlap-segment? bb l)
  (let* ((bx1 (bbox-x bb)) (by1 (bbox-y bb)) (bx2 (+ bx1 (bbox-w bb))) (by2 (+ by1 (bbox-h bb)))
        (l1 (segment bx1 by1 bx1 by2))
        (l2 (segment bx1 by1 bx2 by1))
        (l3 (segment bx2 by1 bx2 by2))
        (l4 (segment bx2 by2 bx1 by2))
     )
    (or
      (segments-cross? l l1)
      (segments-cross? l l2)
      (segments-cross? l l3)
      (segments-cross? l l4))))
