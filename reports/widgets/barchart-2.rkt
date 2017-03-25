#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt" (for-syntax "../../lib/syntax.rkt" "../../lib/hash.rkt" "../../lib/controls.rkt"))
(require "../styles.rkt")

(require "../../lib/debug.rkt")

(provide (all-defined-out))

(define-macro (barchart-2 . body) ; STX define-macro form should have the same coloring scheme as define form
  (let* ( [@args (zor (apply hash body) (hash))]
          [data (hash-ref @args 'data null)]
          [title (hash-ref @args 'title "")]
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
            'x-axis (hash 'h 0 'pos 'bottom 'font-size 12)
            'bars (hash 'gap 1))]
        [@layout (hash-union @layout default-layout)]

        [default-labels
          (hash
            'y-axis (hash 'label-direction 'vertical 'start 0 'text "Y" 'tick-offset 10)
            'x-axis (hash 'text "X" 'tick-offset 5 'orientation 0))]
        [@labels (hash-union @labels default-labels)]

        [default-styles
          (hash
            'y-axis (hash 'font-size 12)
            'x-axis (hash 'font-size 12))]
        [@styles (hash-union @styles default-styles)]

        [widget-x (hash-path @layout 'widget 'x)]
        [widget-y (hash-path @layout 'widget 'y)]
        [widget-w (hash-path @layout 'widget 'w)]
        [widget-h (hash-path @layout 'widget 'h)]

        [title-x 0]
        [title-pos (hash-path @layout 'title 'pos)]
        [title-w widget-w]
        [title-font-size (if (< widget-h 360)
                            (/r widget-h 12)
                            30)]
        [TITLE_H (thenot
                    title-pos 'hidden
                    (zor
                      (hash-path @layout 'title 'h)
                      (* 3 title-font-size)))]

        [y-axis-pos (hash-path @layout 'y-axis 'pos)]
        [y-axis-y (the title-pos 'top TITLE_H)]
        [Y-AXIS_W (thenot y-axis-pos 'hidden (hash-path @layout 'y-axis 'w))]
        [y-axis-font-size (hash-path @styles 'y-axis 'font-size)]
        [y-axis-label (hash-path @labels 'y-axis 'text)]
        [y-axis-label-direction (hash-path @labels 'y-axis 'label-direction)]
        [y-axis-start (hash-path @labels 'y-axis 'start)]
        [y-axis-tick-offset (@. @labels.y-axis.tick-offset)]

        [x-axis-pos (hash-path @layout 'x-axis 'pos)]
        [x-axis-x (the y-axis-pos 'left Y-AXIS_W)]
        [X-AXIS_H (thenot x-axis-pos 'hidden (hash-path @layout 'x-axis 'h))]
        [x-axis-font-size (hash-path @styles 'x-axis 'font-size)]
        [x-axis-label (@. @labels.x-axis.text)]
        [x-axis-tick-offset (@. @labels.x-axis.tick-offset)]
        [x-axis-orientation (@. @labels.x-axis.orientation)]

        [bars-gap (hash-path @layout 'bars 'gap)]
        [bars-x (the y-axis-pos 'left Y-AXIS_W)]
        [bars-y (+ (the title-pos 'top TITLE_H) (the x-axis-pos 'top X-AXIS_H))]
        [bars-w (- widget-w Y-AXIS_W)]
        [bars-h (- widget-h TITLE_H X-AXIS_H)]

        [title-y (the title-pos 'bottom (+ bars-h X-AXIS_H))] ;(if (equal? title-pos 'bottom) (+ bars-h X-AXIS_H) 0)

        [y-axis-x (the y-axis-pos 'right bars-w)]
        [y-axis-h bars-h]

        [x-axis-y (+ (the title-pos 'top TITLE_H) (the x-axis-pos 'bottom bars-h))]
        [x-axis-w bars-w]

        [text-l (text-length title)]
        [text-h (text-height title)]
        [text-x  (+ title-x (/r (- title-w text-l) 2))]
        [text-y (+ title-y (/r (+ TITLE_H text-h) 2))]

        [bar-class (zor (hash-path @styles 'bar 'class) "none")]
        [bar-class (split bar-class " ")]
        [bar-class (if (= (length bar-class) 1) (car bar-class) bar-class)]
        [bar-class-hover (zor (hash-path @styles 'bar 'class-hover) "none")]
        [bar-class-hover (split bar-class " ")]
        [bar-class-hover (if (= (length bar-class-hover) 1) (car bar-class-hover) bar-class-hover)]

        [bar-colors (zor (hash-path @styles 'bar 'colors) "black")]
        ;[_ (__t (format "bar-colors: ~a~n" bar-colors))]

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
        [scale-factor (exact->inexact (/ bars-h (apply max data-n1)))]
        ; data normalization (scaling), second step
        [data-n2 (map (λ (y) (*r y scale-factor)) data-n1)])

    ;; barchart widget
    (g (@ 'class "barchart" 'transform (svg/translate widget-x widget-y))

      ;; title block
      (unless (equal? title-pos 'hidden)
        (g
          (@ 'id "title" 'transform (svg/translate title-x title-y))
          (text (@
                  'x (h-centrify title-w (@ 'text title 'font-size title-font-size))
                  'y (v-centrify TITLE_H title-font-size)
                  'font-size title-font-size)
                title)))

      ;; y-axis block
      (unless (equal? y-axis-pos 'hidden)
        (g
          (@ 'id "y-axis" 'transform (svg/translate y-axis-x y-axis-y))
          (let* ( (label-x 0)
                  (label-y (+ (v-centrify y-axis-h) (/r (text-length (@ 'text y-axis-label 'font-size y-axis-font-size)) 2)))
                  (ang (the y-axis-label-direction 'vertical "-90")))

            (str
              (text (@ 'x label-x 'y label-y 'font-size y-axis-font-size
                        'transform (svg/rotate ang label-x label-y)) ; TODO: make it more elegant, don't transform when not vertical
                    y-axis-label)

              (let ((ticks (fractize
                              (or y-axis-start (apply min data-n1))
                              (apply max data-n1)
                              (inc (/c bars-h 60)))))

                (for/fold/idx
                  (s "")
                  (tick ticks)
                    (let* ((tick-y (- y-axis-h (* scale-factor tick)))
                          (tick-w (if (> Y-AXIS_W 60) 15 (* 0.25 Y-AXIS_W)))
                          (tick-offset y-axis-tick-offset)
                          (tick-label-offset (+ (* 2 tick-offset) tick-w))
                          (tick-label-length (text-length (@ 'text (str tick) 'font-size y-axis-font-size))))
                      (if (> tick-y 0)
                        (str s
                          (text (@
                                  'x (- Y-AXIS_W tick-label-offset tick-label-length)
                                  'y (+ tick-y (/r y-axis-font-size 2))
                                  'font-size y-axis-font-size) (str tick))
                          (line 'x1 (- Y-AXIS_W tick-w tick-offset)
                                'y1 tick-y
                                'x2 (- Y-AXIS_W tick-offset)
                                'y2 tick-y))
                        s))))))))

      ;; x-axis block
      (unless (equal? x-axis-pos 'hidden)
        (g
          (@ 'id "x-axis" 'transform (svg/translate x-axis-x  x-axis-y))
          (text (@
                  'x (h-centrify x-axis-w (@ 'text x-axis-label 'font-size x-axis-font-size))
                  'y X-AXIS_H
                  'font-size x-axis-font-size)
                x-axis-label)
          (unless (nil? labels)
            (for/fold/idx
              (s "")
              (lbl (map str labels))
                (str s
                    (let ((x (+
                                (* $idx bar-w)
                                (* (dec $idx) bars-gap)))
                                ;(h-centrify bar-w (@ 'text lbl 'font-size x-axis-font-size))))
                          (y (+ x-axis-tick-offset x-axis-font-size)))
                      (text
                        (@
                          'x x
                          'y y
                          'font-size x-axis-font-size
                          'transform (svg/rotate x-axis-orientation x y))
                        lbl)))))))

      ;; bars block
      (g
        (@ 'id "bars" 'transform (svg/translate bars-x bars-y))
        (for/fold/idx
          (s "")
          (h data-n2)
            (str s
                (rect x (* $idx (+ bar-w bars-gap))
                  y (- bars-base h)
                  width bar-w
                  height h
                  ;class (if (list? bar-class)
                  ;            (list-ref bar-class (+c 0 $idx (length bar-class)))
                  ;            bar-class)
                  style (format
                          "fill:~a"
                          (if (list? bar-colors)
                              (list-ref bar-colors (+c 0 $idx (length bar-colors)))
                              bar-colors))
                  ;onmouseover (str "hover(evt, " (alist->json (list '("fill" "blue") '("opacity" "0.5"))))
                  ;onmouseout "hout(evt)"
                  )))))))
