#lang racket

(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../../scrap/google.rkt")
(require "../../scrap/esri/shp.rkt")
(require "../../graphics/color.rkt")
(require "../../graphics/svg.rkt")
(require "../../graphics/geometry.rkt")
(require "../../graphics/layout.rkt")
(require "../../graphics/fonts.rkt")

(provide (all-defined-out))

(define kukuevsk (@ "name" "Кукуевск" "x" 10 "y" 10))

(define text-style-0 "font-size: 8; font-weight: bold")
(define text-style-1 "font-size: 6")
(define text-style-2 "font-size: 6; fill: #666666")
(define text-style-3 "font-size: 4; fill: #000000")

(define (clean-disabled h-of-hs)
  (hash-filter (λ (k v) (not (equal? (hash-ref v "_disabled" #f) "1"))) h-of-hs))

(define (routes-map
          #:gdocs-routes gdocs-routes
          #:gdocs-places gdocs-places
          #:ft ft
          #:geobase (geobase #f)
          #:line-width (line-width 1.5)
          #:type:style (type:style #f)
          #:smooth-factor (smooth-factor #f)
     )
  (let*
    (
      (places
          (google-spreadsheet/get-tsv gdocs-places))        
      (places-names (map (λ (x) (hash-ref x "name")) (hash-values places)))
      (places (hash-group-by places "name"))
      (routes (google-spreadsheet/get-tsv gdocs-routes))
      (routes (clean-disabled routes))
 )
      (str
        ;;; map background
        (when/str geobase
          (image 'xlink:href geobase 'x 0 'y 0 'width "1191" 'height "842"))

        ;;; road lines
        (for/fold
          ((s ""))
          (((idx route) routes))
          (str
            s
            (let*
              ((rs (hash-ref route "Маршрут" ""))
              (rs (split rs ";")))
              (for/fold
                ((ss ""))
                ((points rs))
                (str
                  ss
                  (let*
                    (
                    (points (re-substitute points '("^\\s+" "\\s+$" " ," ", " "\t" "\r" "\n") '("" "" "," "," "" "" "")))
                    (points (split points ","))
                    (points (map (λ (x) (if (= 1 (count-element places-names x))
                                                (hash-insert (first (hash-values (hash-ref places x))) (cons 'name x))
                                                #f))
                                  points))
                    (points (clean false? points)) ; remove unknown locations
                    (named-points
                      (map
                        (λ (x) (cons
                                  (hash-ref x 'name)
                                  (ft
                                    (cons (strnumber->number (hash-ref x "lon" 0)) (strnumber->number (hash-ref x "lat" 0))))))
                        points))
                    (type (hash-ref route "Тип" ""))
                 )
                      (when/str
                        (> (length points) 1)
                          ; points names for debug purposes
                          ;(for/fold
                          ;  ((ss ""))
                          ;  ((p named-points))
                          ;  (str
                          ;    ss
                          ;    (text (@ 'x (cadr p) 'y (cddr p) 'style "font-size: 7") (car p))))
                          (for/fold
                            ((ss ""))
                            ((style (hash-ref type:style type)))
                            (str
                              ss
                              (path
                                ;'d (points->smooth-path points #:fct smooth-factor)
                                'd (if smooth-factor
                                      (points->smooth-path (map cdr named-points) #:fct smooth-factor)
                                      (points->path (map cdr named-points)))
                                'style style)))
                   )
)))))))))
