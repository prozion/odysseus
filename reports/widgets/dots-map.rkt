#lang racket

(require "../../lib/load/all.rkt")
(require "../../scrap/csv.rkt")
(require "../../scrap/google.rkt")
(require "../../scrap/esri/shp.rkt")
(require "../../graphics/color.rkt")
(require "../../graphics/svg.rkt")
(require "../../graphics/layout.rkt")
(require "../../graphics/fonts.rkt")

(require compatibility/defmacro)

(provide (all-defined-out))

(define (dots-map
          #:gdocs-places gdocs-places
          ;#:map-base (map-base "fareast_greyscale_3x.png")
          #:map-base (map-base "fareast_south_green_3x")
          #:map-ft ft
          #:value-parameter value-parameter
          #:default-value-parameter (default-value-parameter 0)
          #:fade (fade 0)
          #:title (title "")
          #:diameter-range (diameter-range '(1 . 1))
          #:opacity-range (opacity-range '(1 . 1))
          #:hue-range (hue-range '(0 . 0))
          #:light-range (light-range '(50 . 50))
     )
  (let* (
        (places (google-spreadsheet/get-tsv gdocs-places))
        (places (hash-clean (λ (k v) (equal? (hash-ref v value-parameter) "")) places))
        (max-value (apply max (map (λ (x) (strnumber->number (hash-ref x value-parameter default-value-parameter))) (hash-values places))))
     )
    (str
      ;;; map background
      (image 'xlink:href map-base 'x 0 'y 0 'width "1191" 'height "842")
      (rect 'x 0 'y 0 'width "1191" 'height "842" 'style (format "fill: white; opacity: ~a" fade)) ; fade basemap
      (for/fold
        ((s ""))
        (((k v) places))
        (let* (
              (lon (strnumber->number (hash-ref v "lon" "0")))
              (lat (strnumber->number (hash-ref v "lat" "0")))
              (xy (ft (cons lon lat)))
              (x (car xy))
              (y (cdr xy))
              (value (strnumber->number (hash-ref v value-parameter default-value-parameter)))
              (d (get-proportion diameter-range (sqrt value) (sqrt max-value)))
              (r (/ d 2.0))
              (opacity (get-proportion opacity-range value max-value))
              (hue (get-proportion hue-range value max-value))
              (light (get-proportion light-range value max-value))
              (fill (hsv->rgbstr (list hue 100 light)))
           )
        (str
          s
          (circle
            'id k
            'cx x
            'cy y
            'r r
            'style (format "fill: ~a; opacity: ~a" fill opacity))))))))
