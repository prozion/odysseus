#lang racket

(require compatibility/defmacro)
(require "../graphics/svg.rkt")
(require "../graphics/fonts.rkt")
(require "../graphics/layout.rkt")
(require "../utils/all.rkt" (for-syntax "../utils/syntax.rkt" "../utils/hash.rkt" "../utils/controls.rkt"))
(require "styles.rkt")

(provide (all-defined-out))

(define-macro (barchart . body) ; STX define-macro form should have the same coloring scheme as define form
  (let* ( [@args (zor (apply hash body) (hash))]
          [data (hash-ref @args 'data null)]
          [title (hash-ref @args 'title "")]
          ;; TODO: add just the lacking default parameters, new operation in hash.rkt (hash-union <core-hash> <rest-hash>)
          [layout (hash-ref @args 'layout (hash))]
          [labels (hash-ref @args 'labels (hash))]
          [styles (hash-ref @args 'styles (hash))]
          [normalize-mode (hash-ref @args 'normalize-mode ''trunc-at-zero)])
    `(barchart-f
        #:layout ,layout
        #:labels ,labels
        #:styles ,styles
        #:data ,data
        #:title ,title
        #:normalize-mode ,normalize-mode)))

;(define (barchart-f2 @layout @styles data title normalize-mode)
;  (println @layout))

; layout -- positions + sizes of blocks in the widget
; $layout = (widget: (x y w h), title: (h pos[top|bottom]), y-axis: (w pos[left|right]), x-axis: (h pos[top|bottom]), bars: (gap))
; $styles = (bars: (class), title: (font-family font-weight font-size), y-axis-text (font-family font-weight font-size), x-axis-text (font-family font-weight font-size)
(define (barchart-f
            #:layout @layout
            #:labels @labels
            #:styles @styles
            #:data data
            #:title title
            #:normalize-mode normalize-mode)
  (define (normalize data factor)
    (map factor data))
  (let* (
        [default-layout
          (hash
            'widget (hash 'x 0 'y 0 'w 800 'h 600)
            'title (hash 'h 0 'pos 'top)
            'y-axis (hash 'w 0 'pos 'left)
            'x-axis (hash 'h 0 'pos 'bottom)
            'bars (hash 'gap 1))]
        [@layout (hash-union @layout default-layout)]

        [widget-x (hash-path @layout 'widget 'x)]
        [widget-y (hash-path @layout 'widget 'y)]
        [widget-w (hash-path @layout 'widget 'w)]
        [widget-h (hash-path @layout 'widget 'h)]

        [title-x 0]
        [title-pos (hash-path @layout 'title 'pos)]
        [title-w widget-w]
        [title-font-size (/r widget-h 12)]
        [title-h (thenot
                    title-pos 'hidden
                    (zor
                      (hash-path @layout 'title 'h)
                      (* 3 title-font-size)))]

        [y-axis-pos (hash-path @layout 'y-axis 'pos)]
        [y-axis-y (the title-pos 'top title-h)]
        [y-axis-w (thenot y-axis-pos 'hidden (hash-path @layout 'y-axis 'w))]
        [y-axis-h (- widget-h title-h)]
        [y-axis-label (hash-path @labels 'y-axis 'text)]
        [y-axis-label-direction (hash-path @labels 'y-axis 'direction)]

        [x-axis-pos (hash-path @layout 'x-axis 'pos)]
        [x-axis-x (the y-axis-pos 'left y-axis-w)]
        [x-axis-h (thenot x-axis-pos 'hidden (hash-path @layout 'x-axis 'h))]
        [x-axis-font-size (hash-path @styles 'x-axis 'font-size)]
        [x-axis-label (hash-path @labels 'x-axis 'text)]

        [bars-gap (hash-path @layout 'bars 'gap)]
        [bars-x (the y-axis-pos 'left y-axis-w)]
        [bars-y (+ (the title-pos 'top title-h) (the x-axis-pos 'top x-axis-h))]
        [bars-w (- widget-w y-axis-w)]
        [bars-h (- widget-h title-h x-axis-h)]

        [title-y (the title-pos 'bottom (+ bars-h x-axis-h))] ;(if (equal? title-pos 'bottom) (+ bars-h x-axis-h) 0)

        [y-axis-x (the y-axis-pos 'right bars-w)]

        [x-axis-y (+ (the title-pos 'top title-h) (the x-axis-pos 'bottom bars-h))]
        [x-axis-w bars-w]

        [text-l (text-length title)]
        [text-h (text-height title)]
        [text-x  (+ title-x (/r (- title-w text-l) 2))]
        [text-y (+ title-y (/r (+ title-h text-h) 2))]

        [bar-class (zor (hash-path @styles 'bar 'class) "none")]

        [amount (length data)]
        [bar-w (/r (- bars-w (* (- amount 1) bars-gap)) amount)]
        [bars-base bars-h]
        [labels (if (alist? data) (firsts data) null)]
        [data (if (alist? data) (seconds data) data)]
        [data-max (apply max data)]
        [data-min (apply min data)]
        ;; data normalization, first step
        [data-n1
          (case normalize-mode
            ([min-to-zero]
              (if (< data-min 0)
                (map (λ (x) (+ x (abs data-min))) data)
                (map (λ (x) (- x data-min)) data)))
            ([trunc-at-zero]
              (map (λ (x) (if (< x 0) 0 x)) data))
            (else data))]
        ; data normalization (scaling), second step
        [data-n2 (map (λ (y) (/r (* y bars-h) data-max)) data-n1)])

    ;; barchart widget
    (g (@ 'class "barchart" 'transform (svg/translate widget-x widget-y))

      ;; title block
      (unless (equal? title-pos 'hidden)
        (g
          (@ 'id "title" 'transform (svg/translate title-x title-y))
          (text (@
                  'x (h-centrify title-w (@ 'text title 'font-size title-font-size))
                  'y (v-centrify title-h title-font-size)
                  'font-size title-font-size)
                title)))

      ;; y-axis block
      (unless (equal? y-axis-pos 'hidden)
        (g
          (@ 'id "y-axis" 'transform (svg/translate y-axis-x  y-axis-y))
          (let* ((x 0)
                (y (v-centrify y-axis-h))
                (ang (the y-axis-label-direction 'vertical "-90")))
            (text (@ 'x x 'y y
                      'transform (svg/rotate ang x y)) ; TODO: make it more elegant, don't transform when not vertical
                  y-axis-label))))

      ;; x-axis block
      (unless (equal? x-axis-pos 'hidden)
        (g
          (@ 'id "x-axis" 'transform (svg/translate x-axis-x  x-axis-y))
          (unless (nil? labels)
            (for/fold/idx
              (s "")
              (lbl (map str labels))
                (str s
                    (text
                      (@
                        'x (+
                              (* $idx bar-w)
                              (* (dec $idx) bars-gap)
                              (h-centrify bar-w (@ 'text lbl 'font-size x-axis-font-size)))
                        'y (v-centrify x-axis-h)
                        'font-size x-axis-font-size)
                      lbl))))))

      ;; bars block
      (g
        (@ 'id "bars" 'class bar-class 'transform (svg/translate bars-x  bars-y))
        (for/fold/idx
          (s "")
          (h data-n2)
            (str s
                (rect x (* $idx (+ bar-w bars-gap))
                  y (- bars-base h)
                  width bar-w
                  height h
                  )))))))
