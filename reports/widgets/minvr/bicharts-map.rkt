#lang racket

(require "_common.rkt")
(require "../piechart.rkt")
(require "../../../lib/all.rkt")
(require "../../../scrap/google.rkt")
(require "../../../graphics/color.rkt")
(require "../../../graphics/svg.rkt")
(require "../../../graphics/layout.rkt")
(require "../../../graphics/fonts.rkt")
(require racket/set)

(provide (all-defined-out))

(define kukuevsk (@ "name" "Кукуевск" "x" 10 "y" 10))

(define text-style-0 "font-size: 8; font-weight: bold")
(define text-style-1 "font-size: 6")
(define text-style-2 "font-size: 6; fill: #666666")
(define text-style-3 "font-size: 4; fill: #000000")

(define (bicharts-map
          #:gdocs-projects gdocs-projects
          #:gdocs-places gdocs-places
          #:ft ft
          #:geobase (geobase #f)
          #:column-names (colnames #f)
          #:clean-lambda-kv (clean-lambda-kv #f) ; (λ (k v) (equal? (hash-ref v "_disabled" #f) "1"))
          #:bi-colors (bi-colors '("blue" "red"))
          #:bars-width (bars-width 10)
          #:scale-factor (scale-factor 1000) ; 1 px = 1 миллиард рублей
          #:bars-style (bars-style "")
          #:rotation (rotation 0)
     )
  (let*
    (
      (places
          (google-spreadsheet/get-tsv gdocs-places))
      (projects (google-spreadsheet/get-tsv gdocs-projects))
      (projects (hash-clean clean-lambda-kv projects))
      (projects-by-ga (hash-group-by projects (@. colnames.growth-area)))
      (total-budget (λ (ps) (for/fold ((s 0)) ((p (hash-values ps))) (+ s (strnumber->number (hash-ref p (@. colnames.total-budget) 0))))))
      (actual-gas (opt/uniques
                    (map
                      (λ (v) (hash-ref v (@. colnames.growth-area)))
                      (hash-values projects))))
 )
      (str
        ;;; map background
        (when/str geobase
          (image 'xlink:href geobase 'x 0 'y 0 'width "1191" 'height "842"))

        ;;; road lines
        (for/fold
          ((s ""))
          ((ga actual-gas))
          (str
            s
            (let*
              (
              (ga-place (find-place places ga kukuevsk))
              (lon (strnumber->number (hash-ref ga-place "lon" 0)))
              (lat (strnumber->number (hash-ref ga-place "lat" 0)))
              (xy (ft (cons lon lat)))
              (x (car xy))
              (y (cdr xy))
              ;(x (- x0 bars-width))
              (budgets (for/fold
                          ((bs (cons 0 0))) ; state-budgets . private-budgets
                          ((p (hash-values (hash-ref projects-by-ga ga))))
                          (cons
                            (+ (car bs) (strnumber->number (hash-ref p (@. colnames.total-budget) "0")))
                            (+ (cdr bs) (strnumber->number (hash-ref p (@. colnames.private-budget) "0"))))))
              (scale-factor (* 1.0 scale-factor))
              (state-budget (car budgets))
              (private-budget (cdr budgets))
              (w-state (/ state-budget scale-factor))
              (w-private (/ private-budget scale-factor))
              (state-budget-print (format-number "ddd ddd ddd" (int state-budget)))
              (private-budget-print (format-number "ddd ddd ddd" (int private-budget)))
              (state-color (first bi-colors))
              (private-color (second bi-colors))
              (r (sqrt (+ w-state w-private)))
              (ga-name (hash-ref ga-place "ga" "???"))
           )
                (str
                  ;(for/fold/idx
                  ;  (s "")
                  ;  (i '(1000 5000 10000 20000 50000 100000 500000 1000000))
                  ;  (str
                  ;    s
                  ;    (text (@ 'x 2050 'y (* 50 $idx)) i)
                  ;    (circle 'cx 2000 'cy (* 50 $idx) 'r (sqrt (/ i scale-factor)) 'style "fill: green")))
                  (when/str
                    (or (> (car budgets) 0) (> (cdr budgets) 0))
                        ;(g (@ 'transform (svg/rotate rotation x y))
                          ;(circle 'cx x 'cy y0 'r 10 'style "fill: #666; opacity: 0.4")
                          ;(rect 'x (- x w-state) 'y y 'width w-state 'height bars-width 'style (format "fill: ~a; ~a" (nth bi-colors 1) bars-style))
                          ;(rect 'x x 'y y 'width w-private'height bars-width 'style (format "fill: ~a; ~a" (nth bi-colors 2) bars-style)))
                        ;(g
                        ;  (circle 'cx x0 'cy y 'r 7 'style "fill: none; stroke: #666; stroke-width: 1.2; opacity: 0.8")
                        ;  (rect 'x x 'y (- y w-state) 'width bars-width 'height w-state 'style (format "fill: ~a; ~a" (nth bi-colors 1) bars-style))
                        ;  (rect 'x (+ x bars-width) 'y (- y w-private) 'width bars-width 'height w-private  'style (format "fill: ~a; ~a" (nth bi-colors 2) bars-style)))
                        ;(g
                        ;  (circle 'cx x 'cy y 'r (sqrt w-state) 'style (format "fill: ~a; ~a" (nth bi-colors 1) bars-style))
                        ;  (circle 'cx x'cy y 'r (sqrt w-private) 'style (format "fill: ~a; ~a" (nth bi-colors 2) bars-style)))
                        (g
                          (text (@ 'x (+ x 3) 'y (- y 3) 'style "font-size: 8; font-family: Arial") ga-name)
                          (text (@ 'x (+ x 3) 'y (+ y 10) 'style (format "font-size: 8; font-weight: bold; font-family: Arial; fill: ~a" (color/shadow private-color -30))) private-budget-print)
                          (text (@ 'x (+ x 5 (text-length private-budget-print #:font-size 8)) 'y (+ y 10) 'style (format "font-size: 8; font-weight: bold; font-family: Arial; fill: ~a" (color/shadow state-color -30))) state-budget-print)
                          (piechart
                            #:x x
                            #:y y
                            #:r r
                            #:data (list w-private w-state)
                            #:gap 1
                            #:colors (reverse bi-colors)
                            #:style "opacity: 0.5;"))
           ))))))))
