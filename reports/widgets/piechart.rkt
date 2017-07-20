#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/color.rkt")
(require "../../lib/all.rkt")

(provide piechart)

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
            #:donut (donut #f)
            #:donut-style (donut-style "fill: white; stroke: black; stroke-width: 0.5")
            #:style (style "stroke: none;")
            #:style-circle (style-circle "stroke: black; stroke-width: 0; fill: none")
            #:colors (colors #f)
            #:color-raw-model (color-raw-model 'cmyk-based)

            #:debug (debug #f) ; debug info passed here if any
        )
  ;(circle 'cx x 'cy y 'r r ...)
  (let* (
          (data (if data data (gen 1 12)))
          (colors
            (if (list? colors)
              colors
              (case color-raw-model
                ((cmyk-based) cmyk-based-colors)
                ((rgb-based) rgb-based-colors)
                ((hsv-based) (generate-hsv-colors (length data)))
                (else cmyk-based-colors))))
          (total (apply + data)))
    (if (= total 0)
      ""
      (g
        (call-with-values
          (λ ()
            (for/fold
              ((s (circle 'cx x 'cy y 'r r 'style style-circle)) (φ 0))
              ((d data) (color colors))
              (let* ( (dφ (* 2.0 PI (/ d total)))
                      (swap-direction (if (> dφ PI) 1 0))
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
                                        0 swap-direction 0
                                        x2 y2)
                            'style style)
                      ; if only one part:
                      (circle 'cx x 'cy y 'r r 'style style)
                      )
                    (if donut
                      (circle 'cx x 'cy y 'r (donut r) 'style donut-style)
                      ""))
                  (+ φ dφ)))))
          (λ vs (first vs))))))
)
