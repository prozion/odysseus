#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../lib/all.rkt")

(provide (all-defined-out))

; (piechart
;   #:x x
;   #:y y
;   #:r (* r0 (if (list? fill) (sqrt (length fill)) 1))
;   #:colors fill #:mode 'equal)
(define (piechart
            #:x x
            #:y y
            #:r r
            #:data (data #f)
            #:gap (gap 0)
            #:style (style "stroke: none;")
            #:style-circle (style-circle "stroke: black; stroke-width: 0")
            #:colors (colors '("black")))
  ;(circle 'cx x 'cy y 'r r ...)
  (let* ( (colors (if (list? colors) colors (list colors)))
          (data (if data data (gen 1 (length colors))))
          (total (apply + data)))
    (call-with-values
      (λ ()
        (for/fold
          ((s (circle 'cx x 'cy y 'r r 'style style-circle)) (φ 0))
          ((d data) (color colors))
          (let* ( (dφ (/ (* 2 PI d) total))
                  (x1 (+ x (* r (cos φ))))
                  (y1 (- y (* r (sin φ))))
                  (x2 (+ x (* r (cos (+ φ dφ)))))
                  (y2 (- y (* r (sin (+ φ dφ)))))
                  (style (format "fill: ~a; ~a" color style)))
            (values
              (str
                s
                (if (> (length data) 1)
                  ; if two or more parts:
                  (path 'd (format "M~a,~a L ~a,~a A ~a ~a ~a ~a ~a ~a ~a z"
                                    x y
                                    x1 y1
                                    r r
                                    0 0 0
                                    x2 y2)
                        'style style)
                  ; if only one part:
                  (circle 'cx x 'cy y 'r r 'style style)))
              (+ φ dφ)))))
      (λ vs (first vs))))
)
