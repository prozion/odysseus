#lang racket

(require compatibility/defmacro)
(require "../graphics/svg.rkt")
(require "../graphics/color.rkt")
(require "../graphics/fonts.rkt")
(require "../lib/_all.rkt")

(provide barchart)

(define (f attr default-value)
  (λ (x) (or (hash-ref* x attr #f) default-value)))

(define-syntax (extract-attrname stx)
  (syntax-case stx (λ or hash-ref*)
    ((_ (_ (_) (or (hash-ref* _ attr _) _ ...))) #''attr)
    (else #'#f)))

(define (barchart
            #:x x
            #:width max-width
            #:data data
            #:value (value (f 'value 0))
            #:label (label (f 'label ""))
            #:label-text (label-text (f 'label ""))
            #:color (color (f 'color #f))
            #:gap (gap 0)
            #:bar-width (bar-width 15)
            #:style (style "stroke: none;")
            #:style-circle (style-circle "stroke: black; stroke-width: 0; fill: none")
            #:color-raw-model (color-raw-model 'cmyk-based)
            #:label-font-size (label-font-size 11)
            #:default-color (default-color "#99f")

            #:debug (debug #f) ; debug info passed here if any
     )
  (let* (
          ; geometry
          (label-margin-r 15)

          (data-values (map value data))
          (min-data (apply min (filter (λ (x) (> x 0)) data-values)))
          (instead-of-zero (* min-data 1e-6))
          (data-values (map (λ (x) (if (<= x 0) instead-of-zero x)) data-values))
          (total (apply + data-values))
          ; add (or substitute) value parameter
          (data (map (λ (x y) (hash-set x 'value y))
                      data
                      data-values))
          (value (λ (x) (->number ($ value x))))
          (max-value (apply max (map value data)))
          (k (* 0.8 (/ max-width max-value)))
          ; ... and change value function, whatever complex this function was before, now it simply extracts value from 'value' parameter
          (generated-colors (case color-raw-model
                              ((cmyk-based) cmyk-based-colors)
                              ((rgb-based) rgb-based-colors)
                              ((hsv-based) (generate-hsv-colors (length data)))
                              (else cmyk-based-colors)))
          (data (map (λ (x) (cond ((color x) x)
                                  ((extract-attrname color) (hash-set x (extract-attrname color) (nth generated-colors (indexof data x))))
                                  (else (hash-set x 'color default-color))))
                      data))
          (result
                  (if (= total 0)
                    empty
                    (for/fold
                      ((s empty))
                      ((d data) (n (in-naturals 1)))
                      (let* ( (dx (* 1.0 k (value d)))
                              (x1 x)
                              (y1 (* n (+ bar-width gap)))
                              (text (label-text d))
                              (text-x (+ x1 dx gap gap))
                              (text-y (+ y1 (/ bar-width 1.5)))
                              (text-anchor "start")
                              (style (format "fill: ~a; ~a" (color d) style)))
                        (values
                          (append
                            s
                            (list
                              `(rect
                                  (@
                                    (x ,x1)
                                    (y ,y1)
                                    (width ,dx)
                                    (height ,bar-width)
                                    (style ,style)))
                              `(text (@ (x ,text-x) (y ,text-y)) ,(label d))))))))))
        result))
