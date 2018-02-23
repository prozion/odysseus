#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../lib/load/all.rkt")
(require "barchart-2.rkt")

(provide (all-defined-out))

;(barchart
; #:data '((2006 . 108671) (2007 . 107520) (2008 . 106644))
; #:colors '("black" "grey" "#aa6")
; #:gap 1
; #:title "Storskog the best!")
(define (barchart
            #:data (data #f)
            #:max-extent (max-extent #f)

            #:gap (gap 0)
            #:colors (colors '("black"))
            #:xs-orientation (xs-orientation 0)
            #:title (title "")

            #:height (height 0)
            #:width (width 1000)
            #:y0 (y0 0)
            #:y-ticks (y-ticks #f))
  (let* (
        (x 50)
        (y (+ 10 y0))
        (h height)
        (w width))
    (barchart-2
            data data
            layout (@ 'widget (@ 'x x 'y y 'w w 'h h)
                      'title (@ 'h (/r h 10) 'pos 'top)
                      'y-axis (@ 'w 100 'pos 'left)
                      'x-axis (@ 'h (/r h 10) 'pos 'bottom)
                      'bars (@ 'gap gap))
            scales (@ 'max-extent max-extent)
            labels (@ 'x-axis (@ 'text "" 'orientation xs-orientation)
                      'y-axis (@ 'text "" 'start 0 'ticks y-ticks))
            styles (@ 'bar (@ 'colors colors)
                      'x-axis (@ 'font-size 10)
                      'y-axis (@ 'font-size 10))
            normalize-mode 'trunc-at-zero
            title title)))
