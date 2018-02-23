#lang racket

(require "../../lib/load/all.rkt")
(require "../piechart.rkt")
(require "_common.rkt")
(require "../../scrap/csv.rkt")
(require "../../scrap/google.rkt")
(require "../../scrap/esri/shp.rkt")
(require "../../graphics/color.rkt")
(require "../../graphics/svg.rkt")
(require "../../graphics/layout.rkt")
(require "../../graphics/fonts.rkt")

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

(define (total-column-fgen key)
  (λ (pair)
    (for/fold
      ((s 0))
      (((k v) (cdr pair)))
      (begin
        ;(printf "~a ~a~n" s (hash-ref v key))
        (+ s (strnumber->number (hash-ref v key)))))))

(define colnames-default (@ 'total-budget "Общий бюджет" 'federal-budget "Федеральный бюджет" 'regional-budget "Общий местный бюджет" 'state-project "ГП" 'project-description "Краткое описание проекта"))

(define (piecharts-map
          #:gdocs-projects gdocs-projects
          #:gdocs-places gdocs-places
          #:geobase (geobase "geobase.png")
          #:ft ft

          #:title (title "")
          #:description (description "")

          #:legend1 (legend1 #t)
          #:legend2 (legend2 #t)
          #:barchart-legend (barchart-legend #f)
          #:legend-title (legend-title "")
          #:legend-circles (legend-circles #f)
          #:max-value-type (max-value-type 'absolute)
          #:max-piechart-radius (max-piechart-radius 50)
          #:pointer-step0 (pointer-step0 10)
          #:project-description (project-description #f)

          #:special-places (specials (list))

          #:selected-programs (selected-programs #f)

          #:column-names (colnames #f)
          #:program-names (program-names #f)

          #:piechart-by-category (piechart-by-category (@. colnames-default.total-budget))
          #:filter-by (filter-by #f)
          #:minimal-value (minimal-value 0)

          ;#:piechart-colors (piechart-colors #f)
          #:piechart-colors-clist (piechart-colors-clist #f)
          #:piechart-opacity (piechart-opacity 1)
          #:font-size (font-size 10)
     )
  (let*
    (
      (colnames (if colnames
                          (hash-union colnames colnames-default)
                          colnames-default))
      (total-budget (λ (ps) (for/fold ((s 0)) ((p (hash-values ps))) (+ s (strnumber->number (hash-ref p (@. colnames.total-budget) 0))))))
      (place:total-budget (total-column-fgen (@. colnames.total-budget)))
      (place:total-federal-budget (total-column-fgen (@. colnames.federal-budget)))
      (place:total-regional-budget (total-column-fgen (@. colnames.regional-budget)))
      ;(gp-colors (make-gp-colors piechart-colors))
      (places
          (google-spreadsheet/get-tsv gdocs-places))
      (places (hash-filter
                  (λ (k v)
                    (let ((type (hash-ref v "type" "none")))
                      (indexof? '("city" "hamlet" "locality" "neighbourhood" "town" "village") type)))
                  places))
      (projects (google-spreadsheet/get-tsv gdocs-projects))
        ;"https://docs.google.com/spreadsheets/d/1vvtBIgQL6Zcw41_TyrskmUkUVWU98EUdXkUq76KNuuA/pub?gid=256594381&single=true&output=tsv"))
      (projects (hash-filter (λ (k v) (not (equal? (hash-ref v "_disabled" #f) "1"))) projects))

      (max-value-absolute (total-budget projects))

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

      (projects-hash projects) ; for barchart legend and total numbers

      (max-value-relative (total-budget projects))

      (all-programs (map
                        (λ (x) (hash-ref x (@. colnames.state-project)))
                        (hash-values projects)))
      (programs-by-number (clist-sort (frequency all-programs) (λ (k1 v1 k2 v2) (> v1 v2))))

      ; names for legend
      ;(existed-projects (opt/uniques all-projects))
      (existed-programs (map car programs-by-number))
      ;(nonexisted-programs (minus all-programs existed-programs))

      ; colors, starting from frequent to rare programs
      (programs-colors
        (filter
          (λ (x) (indexof? existed-programs (car x)))
          piechart-colors-clist))


      ;; group by place
      (projects (sort
                  (hash->list
                    (hash-group-by projects "Город"))
                  (λ (a b)
                    (> (place:total-budget a) (place:total-budget b)))))

      (projects (clean (λ (x) (equal? (car x) "")) projects)) ; remove projects without location

      (max-value (if (equal? max-value-type 'relative) max-value-relative max-value-absolute))

      (title (@ 'text title 'x 950 'y 5 'w 400 'h 70))
      (description (@ 'text description 'x 910 'y 450 'w 290 'h 100))
      (legend1 (and legend1
                  (@ 'title legend-title 'x 950 'y 400 'w 400 'h 100
                    'circles (if legend-circles legend-circles (list 100 500 1000 5000 10000 50000 100000))
                    'circles-left-margin 30
                    'circles-right-margin 20
                    'label-bottom-margin 30)))
      (legend2 (and legend2
                    (@ 'title "ЦВЕТОВОЙ КОД" 'x 950 'y 640 'w 400 'h 400)))
      (piecharts (@ 'r0 3))

      (R0 3)
      (RM max-piechart-radius)
      (M max-value)
      (a (* (sqrt M) (- (/ RM R0) 1))) ; scaling factor
      (find-radius (λ (t) (* R0 (+ 1 (/ (* a (sqrt t)) M)))))
 )
      (str
        ;;; map background
        (when/str
          geobase
          (image 'xlink:href geobase 'x 0 'y 0 'width "1191" 'height "842"))

        ;;; title
        (rect 'x (@. title.x) 'y (@. title.y) 'width (@. title.w) 'height (@. title.h) 'style "fill: white; opacity: 0.8")
        (text
          (@ 'x (+ (@. title.x) (/ (@. title.w) 2.0)) 'y (+ 30 (@. title.y)) 'style "font-size: 32; font-weight: bold; font-family: Calibri; fill: #828281; text-anchor: middle")
          (@. title.text))

        ;;; description
        ;(rect 'x (@. description.x) 'y (@. description.y) 'width (@. description.w) 'height (@. description.h) 'style "fill: #efffe8")
        ;(text
        ;  (@ 'x (@. description.x) 'y (@. description.y) 'style "font-size: 14; font-weight: normal; font-family: Calibri; fill: #828281")
        ;  (@. description.text))

        ;;; legend I
        (when/str legend1
          (rect 'x (@. legend1.x) 'y (@. legend1.y) 'width (@. legend1.w) 'height (@. legend1.h) 'style "stroke: #f2f2e8; stroke-width: 3; fill: none;")
          (text
            (@ 'x (+ (@. legend1.x) (/ (@. legend1.w) 2.0)) 'y (+ (@. legend1.y) 20) 'style "font-size: 16; font-weight: bold; font-family: Calibri; fill: #828281; text-anchor: middle")
            (@. legend1.title))
          (for/fold/idx
            (s "")
            (i (@. legend1.circles))
            (let* ((max-r (find-radius (apply max (@. legend1.circles))))
                  (legend-circles-w (- (@. legend1.w) (@. legend1.circles-left-margin) (@. legend1.circles-right-margin)))
                  (legend-circles-x (+ (@. legend1.x) (@. legend1.circles-left-margin)))
                  (legend-circles-y (+ (@. legend1.y) (@. legend1.h) -45))
                  (dx (* 1.0 $idx (/ legend-circles-w (length (@. legend1.circles)))))
                  ;(fill (if (= (length existed-programs) 1) (cdar programs-colors) "#333333"))
                  (fill "#333")
               )
              (str
                s
                (circle
                  'cx (+ legend-circles-x dx)
                  'cy legend-circles-y
                  'style (format "fill: ~a; opacity: 0.4; stroke: black; stroke-width: 0.5" fill)
                  'r (find-radius i))
                (circle
                  'cx (+ legend-circles-x dx)
                  'cy legend-circles-y
                  'style (format "fill: ~a; opacity: 1; stroke: black; stroke-width: 0.3" "white")
                  'r (/ (find-radius i) 3.0))
                (text
                  (@
                    'x (+ legend-circles-x dx)
                    'y (+ (@. legend1.y) (@. legend1.h) -10)
                    'style (format "~a; text-anchor: middle" text-style-1))
                  i)
           ))))
        ;;; legend II
        (when/str legend2
          (rect 'x (@. legend2.x) 'y (@. legend2.y) 'width (@. legend2.w) 'height (+ 100 (* 10 2 (length existed-programs))) 'style "stroke: #f2f2e8; stroke-width: 3; fill: none;") ; fill: #f2f2e8;
          (text
            (@ 'x (+ (@. legend2.x) 30) 'y (+ (@. legend2.y) 20) 'style (format "font-size: ~a; font-weight: bold; font-family: Calibri; fill: #828281" font-size))
            (@. legend2.title))
          (let* ((l (length programs-colors)))
            (for/fold/idx
              (s "")
              (i programs-colors)
              (let*
                (
                  (x (+ (if (< $idx (/ l 2)) 10 (/ (@. legend2.w) 2.0))
                        (@. legend2.x)))
                  (w 24)
                  (h 12)
                  (gap 4)
                  (y (+ (if (< $idx (/ l 2))
                          (@. legend2.y)
                          (- (@. legend2.y) (* (+ h gap) (/ l 2))))
                        40
                        (* (+ h gap) $idx)))
                  (text-y (+ y (* 1.0 h 2/3)))
                  (text-x (+ x w 10))
                  (txt (car i))
                  (style (format "fill: ~a; stroke: none;" (cdr i)))
             )
                (str
                  s
                  (g
                    (rect 'x x 'y y 'width w 'height h 'style style)
                    (text (@ 'x text-x 'y text-y 'style text-style-2) txt)))))))

        ;; barchart legend
        (when/str barchart-legend
          (letrec ((aggregate-projects (λ (projects f)
                                          (for/fold
                                            ((s null))
                                            (((k v) projects))
                                            (f s v))))
                (agg-programs-budget (λ (s v)
                                        (cond
                                          ((null? s) (agg-programs-budget (apply hash (interleave all-programs (gen 0 (length all-programs)))) v))
                                          (else
                                            (let* (
                                                  (cur-gp (hash-ref v (@. colnames.state-project) "?"))
                                                  (cur-budget (hash-ref s cur-gp 0))
                                                  (res (hash-substitute s (cons cur-gp (+ cur-budget (strnumber->number (hash-ref v (@. colnames.total-budget) 0)))))))
                                              res)))))
                (ps (hash->sorted-clist (aggregate-projects projects-hash agg-programs-budget))))
            (for/fold/idx
              (s "")
              (p ps)
              (let* ((x0 900)
                    (y0 400)
                    (label-x 200)
                    (gap 3)
                    (h 15)
                    (base-w 20)
                    (w (cdr p))
                    (w (+ base-w (int (/ w 2000))))
                    (y (+ y0 (* (+ h gap) $idx)))
                    (yt (+ y (* h 0.7)))
                 )
                (str
                  s
                  (text (@ 'x (+ x0 -10) 'y yt 'style (format "font-family: Arial; font-size: ~a; font-weight: normal; text-anchor: end" font-size)) (car p))
                  (rect 'x x0 'y y 'height h 'width w 'style (format "fill: ~a" (clist-ref piechart-colors-clist (car p) "black")))
                  (line 'x1 (+ x0 base-w) 'y1 y 'x2 (+ x0 base-w) 'y2 (+ y h) 'style "stroke-width: 1; stroke: white")
                  (text (@ 'x (+ x0 w 10) 'y yt 'style (format "font-family: Arial; font-weight: bold; font-size: ~a" font-size)) (inexact->exact (ceiling (cdr p))))
             )
           )
         )
       )
     )

        ;;; objects on the map
        (hash-ref
          (for/fold
            ((s (hash 's "" 'debugs (list) 'bboxes (list) 'lines (list) 'places (list)))) ; accumulated svg string, list of bboxes and lines
            ((p projects))
            (let*
                ((k (car p))
                (v (cdr p))
                (place (find-place places k kukuevsk))
                ;(x (->number (hash-ref place "x" 0)))
                ;(y (->number (hash-ref place "y" 0)))
                (lon (->number (hash-ref place "lon" 0)))
                (lat (->number (hash-ref place "lat" 0)))
                (xy (ft (cons lon lat)))
                (x (car xy))
                (y (cdr xy))
                (w (* 2.0 (text-length k #:font-size font-size)))
                (h (* 2.0 (text-height k #:font-size font-size)))
                (style (format "opacity: ~a; stroke: black; stroke-width: 0.5" piechart-opacity))
                (data
                    (sort
                      (map
                        (λ (b)
                          (hash
                            'color
                              (hash-ref (make-hash programs-colors) (hash-ref b (@. colnames.state-project)) "#000000")
                            'data-parameter
                              (strnumber->number (hash-ref b piechart-by-category))))
                        (hash-values v))
                      (λ (c d) (> (hash-ref c 'data-parameter) (hash-ref d 'data-parameter)))))
                (data-parameters (map (λ (x) (hash-ref x 'data-parameter)) data))
                (project-descriptions (map
                                        (λ (x) (cons (hash-ref x piechart-by-category 0) (hash-ref x (@. colnames.project-description))))
                                        (hash-values v)))
                (colors (map (λ (x) (hash-ref x 'color)) data))
                (parameter-sum (apply + data-parameters))

                (debugs (hash-ref s 'debugs))
                (bboxes (hash-ref s 'bboxes))
                (lines (hash-ref s 'lines))
                (places (hash-ref s 'places))
                ;(_ (println (for*/fold ((s (list))) ((i bboxes) (j bboxes)) (pushr s (bbox-overlap? i j)))))
                (dp (next-free-place bboxes x (- y (text-height k)) w h #:step0 pointer-step0 #:step 5 #:debug (list k places))) ; #:segments lines - layout with reduced amount of lines mutual crossing and overlapping names
                ;(_ (println dp))
                (dx (hash-ref dp 'dx))
                (dy (hash-ref dp 'dy))
                (curbbox (hash-ref dp 'bbox))
                (text-anchor (cond
                                ((> dx 0) "start")
                                ((< dx 0) "end")
                                ((= dx 0) "middle")))

                ; labels:
                (dxx (* (sgn dx) 2))
                (dyy (* (sgn dy) 2))
                (total-budget (int (place:total-budget p)))
                (is-special (indexof? specials k))
                (text-style (if is-special text-style-0 text-style-1))
                (dyy (if is-special (* 1.8 dyy) dyy))

                (svg-str (hash-ref s 's))
             )
              ;(when (equal? k "Депутатский") (println gp-colors)) ;)println (hash-ref gp-colors (hash-ref (hash-ref v "508") (@. colnames.state-project)) "#000000")
                  ;(printf "~a~n" (filter (λ (x) (indexof? (list "Находка" "Чугуевка") (car x))) debugs))
                  ;(when (and
                  ;        (indexof? (map car debugs) "Находка")
                  ;        (indexof? (map car debugs) "Чугуевка"))
                  ;  (printf "~a~n" (filter (λ (x) (indexof? (list "Находка" "Чугуевка") (car x))) debugs)))
                  (hash
                    's  (str
                          svg-str
                          (if (and
                                (!= 0 (* x y))
                                (> x 30)
                                (>= total-budget minimal-value)
                           )
                            (str
                              (piechart
                                #:x x
                                #:y y
                                #:r (find-radius parameter-sum)
                                #:data data-parameters
                                #:colors colors
                                #:donut (λ (r) (/ r 3))
                                #:donut-style "fill: #e2e2e2; stroke: black; stroke-width: 0.3"
                                #:style-circle "stroke: black; stroke-width: 0.5; fill: none"
                                #:style style)
                              (g
                                (line 'x1 x 'y1 y 'x2 (+ x dx) 'y2 (+ y dy) 'style "stroke-width: 0.3; stroke: black; opacity: 0.42")
                                (text (@ 'x (+ x dx dxx) 'y (+ y dy dyy) 'style (format "~a; text-anchor: ~a" text-style text-anchor))
                                  k)
                                (text
                                  (@ 'x (+ x dx dxx) 'y (+ y dy dyy font-size) 'style (format "~a; font-weight: bold; fill: #900; text-anchor: ~a" text-style-1 text-anchor)) total-budget)
                                (when/str project-description
                                  (let ((anchor-k (λ (x)
                                                    (case x
                                                      (("start") 1)
                                                      (("middle") 0.5)
                                                      (("end") -1)
                                                      (else 1)
                                                 )))
                                        (font-size 7)
                                        (ladder-factor (λ (num i)
                                                          (if (> num 10)
                                                            (if (< i (/ num 2))
                                                              (* i 5)
                                                              (* (- num i) 5))
                                                            0))))
                                    (for/fold/idx
                                      (ss "")
                                      (pd (sort project-descriptions (λ (a b) (> (->number (car a)) (->number (car b))))))
                                      (str
                                        ss
                                        (text
                                          (@ 'x (+ x dx dxx (ladder-factor (length project-descriptions) $idx)) 'y (+ y dy dyy 10 (* (inc $idx) 10)) 'style (format "~a; font-size: ~a; font-weight: normal; fill: #333; text-anchor: ~a"  text-style-1 font-size text-anchor)) (cdr pd))
                                        (text
                                          (@ 'x (+ x dx dxx (* (anchor-k text-anchor) (+ (text-length (cdr pd) #:font-size font-size) 10 (ladder-factor (length project-descriptions) $idx)))) 'y (+ y dy dyy 10 (* (inc $idx) 10)) 'style (format "~a; font-size: ~a; font-weight: bold; fill: #999; text-anchor: ~a"  text-style-1 font-size text-anchor)) (->int (car pd)))))))

                           )
                              ; bbox debug rectangles:
                              ;(rect 'x (bbox-x curbbox) 'y (bbox-y curbbox) 'width (bbox-w curbbox) 'height (bbox-h curbbox) 'style "fill: #ff00ff; opacity: 0.5")
                              ;(text (@ 'style (format "fill: black; font-size: 11; text-anchor: ~a" text-anchor) 'x (+ x dx 50) 'y (+ y dy 20)) (map int curbbox))
                         )
                            ""))
                    'debugs (pushr debugs (list k curbbox))
                    'places (pushr places (list k (+ x dx) (+ y dy)))
                    'bboxes (pushr bboxes curbbox)
                    'lines (pushr lines (segment x y (+ x dx) (+ y dy)))
               )))
          's)
)))
