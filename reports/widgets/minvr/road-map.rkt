#lang racket

(require "../../../lib/all.rkt")
(require "../piechart.rkt")
(require "../../../scrap/csv.rkt")
(require "../../../scrap/google.rkt")
(require "../../../scrap/esri/shp.rkt")
(require "../../../graphics/color.rkt")
(require "../../../graphics/svg.rkt")
(require "../../../graphics/layout.rkt")
(require "../../../graphics/fonts.rkt")

(provide (all-defined-out))

(define kukuevsk (@ "name" "Кукуевск" "x" 10 "y" 10))

;(define text-style-0 "font-size: 11; font-weight: bold")
;(define text-style-1 "font-size: 9")
;(define text-style-2 "font-size: 9; fill: #666666")
;(define text-style-3 "font-size: 7; fill: #000000")

(define text-style-0 "font-size: 8; font-weight: bold")
(define text-style-1 "font-size: 6")
(define text-style-2 "font-size: 6; fill: #666666")
(define text-style-3 "font-size: 4; fill: #000000")


(define (roads-map
          #:gdocs-projects gdocs-projects
          #:gdocs-places gdocs-places
          #:ft ft
          #:geobase (geobase #f)

          #:title (title "")
          #:description (description "")

          #:max-value-type (max-value-type 'absolute)
          #:pointer-step0 (pointer-step0 10)
          #:project-description (project-description #f)

          #:special-places (specials (list))

          #:selected-programs (selected-programs #f)

          #:column-names (colnames #f)
          #:program-names (program-names #f)

          #:filter-by (filter-by #f)
          #:minimal-value (minimal-value 0)

          ;#:piechart-colors (piechart-colors #f)
          #:colors-clist (colors-clist #f)
          #:line-width (line-width 1.5)
     )
  (let*
    (
      (places
          (google-spreadsheet/get-tsv gdocs-places))
      (projects (google-spreadsheet/get-tsv gdocs-projects))
      (projects (hash-filter (λ (k v) (not (equal? (hash-ref v "_disabled" #f) "1"))) projects))

      ;; take only projects of certain programs
      (projects (if selected-programs
                      (hash-filter (λ (k v) (indexof? selected-programs (hash-ref v (@. colnames.state-project)))) projects)
                      projects))
      ;; take only projects that are marked in some column by some values
      (projects (if filter-by
                  (hash-filter
                    (λ (k v) (indexof? (cdr filter-by) (hash-ref v (car filter-by))))
                    projects)
                  projects))

      (all-programs (map
                        (λ (x) (hash-ref x (@. colnames.state-project)))
                        (hash-values projects)))

      (title (@ 'text title 'x 950 'y 5 'w 400 'h 70))
      (description (@ 'text description 'x 910 'y 450 'w 290 'h 100))
 )
      (str
        ;;; map background
        (when/str geobase
          (image 'xlink:href geobase 'x 0 'y 0 'width "1191" 'height "842"))

        ;;; title
        (rect 'x (@. title.x) 'y (@. title.y) 'width (@. title.w) 'height (@. title.h) 'style "fill: white; opacity: 0.8")
        (text
          (@ 'x (+ (@. title.x) (/ (@. title.w) 2.0)) 'y (+ 30 (@. title.y)) 'style "font-size: 32; font-weight: bold; font-family: Calibri; fill: #828281; text-anchor: middle")
          (@. title.text))

        ;;; road lines
        (for/fold
          ((s ""))
          (((idx project) projects))
          (str
            s
            (let*
              ((towns (hash-ref project "Город" ""))
              (towns (split towns ","))
              (points (map (λ (x) (hash-ref places x #f)) towns))
              (points (clean false? points))
              (points (map (λ (x) (cons (strnumber->number (hash-ref x "lon" 0)) (strnumber->number (hash-ref x "lat" 0)))) points))
              (points (map ft points))
              (program (hash-ref project (@. colnames.state-project) ""))
              ;(_ (println points))
           )
                (when/str
                  (> (length towns) 1)
                    (path
                      'd (format "M~a,~a ~a"
                                  (caar points)
                                  (cdar points)
                                  (for/fold
                                    ((ss ""))
                                    ((point (cdr points)))
                                    (format "~a ~a,~a" ss (car point) (cdr point))))
                      'style (format "stroke: ~a; stroke-width: ~a; opacity: 0.7"
                                    (hash-ref (make-hash colors-clist) program "red")
                                    line-width
                           )
           ))))))))
