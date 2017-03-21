#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../lib/all.rkt")
(require "barchart-2.rkt")

(provide (all-defined-out))

;(barchart
; #:data '((2006 108671) (2007 107520) (2008 106644))
; #:colors '("black" "grey" "#aa6")
; #:gap 1
; #:title "Storskog the best!")
(define (barchart
            #:data (data #f)
            #:gap (gap 0)
            #:colors (colors '("black"))
            #:xs-orientation (xs-orientation 0)
            #:title (title ""))
  (barchart-2
          data data
          layout (@ 'widget (@ 'x 50 'y 10 'w 1000 'h 600)
                    'title (@ 'h 50 'pos 'top)
                    'y-axis (@ 'w 100 'pos 'left)
                    'x-axis (@ 'h 40 'pos 'bottom)
                    'bars (@ 'gap gap))
          labels (@ 'x-axis (@ 'text "" 'orientation xs-orientation)
                    'y-axis (@ 'text "" 'start 0))
          styles (@ 'bar (@ 'colors colors)
                    'x-axis (@ 'font-size 12)
                    'y-axis (@ 'font-size 12))
          normalize-mode 'trunc-at-zero
          title title))
