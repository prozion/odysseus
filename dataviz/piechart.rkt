#lang racket

(require compatibility/defmacro)
(require "../graphics/svg.rkt")
(require "../graphics/color.rkt")
(require "../graphics/fonts.rkt")
(require "../lib/_all.rkt")

(provide piechart)

(define (f attr default-value)
  (λ (x) (or (hash-ref* x attr #f) default-value)))

(define-syntax (extract-attrname stx)
  (syntax-case stx (λ or hash-ref*)
    ((_ (_ (_) (or (hash-ref* _ attr _) _ ...))) #''attr)
    (else #'#f)))

; (piechart
;   #:x x
;   #:y y
;   #:r (* r0 (if (list? fill) (sqrt (length fill)) 1))
;   #:colors fill #:mode 'equal)
(define (piechart
            #:x x
            #:y y
            #:r r
            #:data data
            #:value (value (f 'value 0))
            #:label (label (f 'label ""))
            #:label-text (label-text (f 'label ""))
            #:color (color (f 'color #f))
            #:gap (gap 0)
            #:donut (donut (λ (x) (/ x 8.0)))
            #:donut-style (donut-style "fill: white; stroke: none; stroke-width: 0.5")
            #:style (style "stroke: none;")
            #:style-circle (style-circle "stroke: black; stroke-width: 0; fill: none")
            #:color-raw-model (color-raw-model 'cmyk-based)
            #:label-font-size (label-font-size 11)

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
          ; ... and change value function, whatever complex this function was before, now it simply extracts value from 'value' parameter
          (value (λ (x) ($ value x)))
          (generated-colors (case color-raw-model
                              ((cmyk-based) cmyk-based-colors)
                              ((rgb-based) rgb-based-colors)
                              ((hsv-based) (generate-hsv-colors (length data)))
                              (else cmyk-based-colors)))
          (data (map (λ (x) (cond ((color x) x)
                                  ((extract-attrname color) (hash-set x (extract-attrname color) (nth generated-colors (indexof data x))))
                                  (else (hash-set x 'color y))))
                      data))
          (result
                  (if (= total 0)
                    empty
                    (for/fold
                      ( (s
                          (list
                            `(circle (@ (cx ,x) (cy ,y) (r ,r) (style ,style-circle)))))
                        (φ 0)
                        #:result
                                  (if-not donut
                                    (format-list '(g ~@a) s)
                                    (format-list '(g ~@a ~a) s `(circle (@ (cx ,x) (cy ,y) (r ,(donut r)) (style ,donut-style))))))
                      ((d data))
                      (let* ( (dφ (* 2.0 PI (/ (value d) total)))
                              (swap-direction (if (> dφ PI) 1 0))
                              (x1 (+ x (* r (cos φ))))
                              (y1 (- y (* r (sin φ))))
                              (x2 (+ x (* r (cos (+ φ dφ)))))
                              (y2 (- y (* r (sin (+ φ dφ)))))
                              (text (label-text d))
                              (text-position
                                      (cond
                                        ((and (< x1 x) (< x2 x)) 'left)
                                        ((and (> x1 x) (> x2 x)) 'right)
                                        ((and (< y1 y) (< y2 y)) 'top)
                                        ((and (> y1 y) (> y2 y)) 'bottom)
                                        (else 'top)))
                              (text-anchor (case text-position
                                              ((left) "end")
                                              ((right) "start")
                                              (else "middle")))
                              (half-label-font-size (/ label-font-size 2.0))
                              (quart-label-font-size (/ label-font-size 4.0))
                              (text-len (text-length text #:font-size label-font-size))
                              (label-margin-r (case text-position
                                                ((left right) (/ label-margin-r 2.0))
                                                ((top) (+ half-label-font-size (* label-margin-r 1)))
                                                ((bottom) (+ half-label-font-size (* label-margin-r 1.3)))))
                              (x-coeff (+ 1 (abs (* 0.1 (sin φ)))))
                              (text-x (+ x (* (+ r label-margin-r) (* x-coeff (cos (+ φ (/ dφ 2.0)))))))
                              (text-x (if (equal? text-position 'left) (- text-x text-len) text-x))
                              (text-y (- y (- quart-label-font-size) (* (+ r label-margin-r) (* 0.95 (sin (+ φ (/ dφ 2.0)))))))
                              (style (format "fill: ~a; ~a" (color d) style)))
                        (values
                          (pushr
                            s
                            (if (> (length data) 1)
                              ; if two or more parts:
                              `(path (@ (d ,(format "M~a,~a L ~a,~a A ~a ~a ~a ~a ~a ~a ~a z"
                                                x y
                                                x1 y1
                                                r r
                                                0 swap-direction 0
                                                x2 y2))
                                        (style ,style)))
                              ; if only one part:
                              `(circle (@ (cx ,x) (cy ,y) (r ,r) (style ,style))))
                            `(text (@ (x ,text-x) (y ,text-y)) ,(label d)))
                          (+ φ dφ)))))))
        result))
