#lang racket

(require compatibility/defmacro)
(require "../graphics/svg.rkt")
(require "../graphics/fonts.rkt")
(require "../utils/all.rkt" (for-syntax "../utils/syntax.rkt"))
(require "styles.rkt")

(provide (all-defined-out))

(define-macro (barchart . body) ; STX define-macro form should have the same coloring scheme as define form
  (let* ( [args# (apply hash body)]
          [data (hash-ref args# 'data null)]
          [title (hash-ref args# 'title "")]
          [layout (hash-ref args# 'layout '(x 0 y 0 width 800 height 600))]
          [gap (hash-ref args# 'gap 1)]
          [bar-class (hash-ref args# 'bar-class #f)]
          [normalize-mode (hash-ref args# 'normalize-mode ''trunc-at-zero)])
    `(barchart-f (quote ,data) ,title ,(apply hash layout) ,gap ,bar-class ,normalize-mode)))

; data ~ '(10 2 -3 46 8)
; (barchart '(3 4 8 1 9 5))
(define (barchart-f data title layout gap bar-class normalize-mode)
  (define (normalize data factor)
    (map factor
      (case normalize-mode
        ([min-to-zero]
          (let* (
                [data-min (apply min data)]
                [n-data (if (< data-min 0)
                                    (map (λ (x) (+ x (abs data-min))) data)
                                    (map (λ (x) (- x data-min)) data) )])
            n-data))
        ([trunc-at-zero]
          (map (λ (x) (if (< x 0) 0 x)) data))
        (else data))))
  (let* (
        [dx (hash-ref layout 'x)]
        [dy (hash-ref layout 'y)]
        [box-w (round (hash-ref layout 'width))]
        [box-h (round (hash-ref layout 'height))]
        [title-font-size (/r box-h 10)]
        [title-block-h (if title (* 3 title-font-size) 0)]
        [top-margin (+ dy title-block-h)]
        [text-l (text-length title)]
        [text-x  (+ dx (/r (- box-w text-l) 2))]
        [text-y (- title-block-h (text-height title))]
        [amount (length data)]
        [bar-w (/r (- box-w (* (dec amount) gap)) amount)]
        [base (+ box-h dy)]
        [data-max (apply max data)]
        [factor (λ (x) (/r (* x (- box-h title-block-h)) data-max))])
    (g (@ 'class "barchart")
      (if title (text (@ 'x text-x 'y text-y 'font-size title-font-size) title) null)
      (for/fold/idx
        (s "")
        (h (normalize data factor))
          (str s
              (rect x (+ dx (* $idx (+ bar-w gap)))
                y (- base h)
                width bar-w
                height h
                ))))))
