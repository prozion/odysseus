#lang racket

(require "../barchart.rkt")

(parameterize ([current-namespace (make-base-namespace)]) ;; STX prameterize ~let
  (namespace-require "widgets/barchart.rkt") ;; STX namespace-require ~require
  (namespace-require "utils/hash.rkt")

  (define v-min '(barchart data '(1 2 3 4 7)))

  ;(define v2 '(barchart
  ;  data (108671 107520 106644 108708 104584 109535 140855 195813 252055 320042 161727)
  ;  layout (@ widget (@ x 100 y 100 w 600 h 400))
  ;  title "Storskog bordercrossings by year"))

  (define v-full
    '(barchart
        data '(1 2 3 4 5)
        layout (@ 'widget (@ 'x 100 'y 100 'w 600 'h 400)
                  'title (@ 'h 100 'pos 'top)
                  'y-axis (@ 'w 30 'pos 'left)
                  'x-axis (@ 'h 20 'pos 'bottom)
                  'bars (@ 'gap 2))
        styles (@ 'bar (@ 'class 'some-bar-class))
        normalize-mode 'min-to-zero
        title "Storskog bordercrossings by year"))

  (printf "~a:~n~n~a~n~n~n~n" v-min (eval v-min))
  ;(printf "~a:~n~a~n~n" v2 ,v2)
  (printf "~a:~n~n~a~n~n~n~n" v-full (eval v-full))
)
