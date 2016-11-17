#lang racket

(require "../utils/hash.rkt")

(provide (all-defined-out))

(define @base-properties
    (@
        ; fonts
        'font-family "Arial"
        'font-size 12
        'font-style "normal"

        ;colors
        'fill "black"
    )
)

;(define @base-classes
;    (@
;        'bar-base (@
;          'fill "red"
;          'opacity "0.5"
;        )
;    )
;)

(define (@base-property property-name)
  (hash-ref @base-properties property-name #f))

;(define (base-class: class-name)
;  (hash-ref @base-classes class-name))
