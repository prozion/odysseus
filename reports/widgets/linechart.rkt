#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")

(provide (all-defined-out))

(define (left-control-point x1 y1 x2 y2 x3 y3 #:alpha (alpha 0.3))
  (list
    (- x2 (*r alpha (- x2 x1)))
    (- y2 (*r alpha (- x2 x1) (/ (- y3 y1) (- x3 x1))))))

(define (get-control-points xs ys)
  (cond
    ((or (< (length xs) 3) (< (length ys) 3)) #f)
    ((or (= (length xs) 3) (= (length ys) 3)) (list
                                                (left-control-point (first xs) (first ys) (second xs) (second ys) (third xs) (third ys))
                                                (list (last xs) (last ys))))
    (else
      (let*
        ((xtri (take xs 3))
        (ytri (take ys 3))
        (x1 (first xtri))
        (y1 (first ytri))
        (x2 (second xtri))
        (y2 (second ytri))
        (x3 (third xtri))
        (y3 (third ytri)))
      (push (left-control-point x1 y1 x2 y2 x3 y3) (get-control-points (cdr xs) (cdr ys)))))))

(define (linechart-single
          xs
          data
          #:color (color #f)
          #:stroke-width (stroke-width #f)
          ; curve presentation options:
          #:smooth (smooth #t)
          #:dots (dots #f)
          #:label (label "")
          ; layout
          #:layout (layout (@ 'width PLOT_W 'height PLOT_H))
          ; scaling
          #:k (k 1))
  (let*
      ((width (@. layout.width))
      (height (@. layout.height))
      (data (map ->number data))
      (dx (/r width (length data)))
      ;(xs (range 0 width dx))
      (scaled-data (map (λ (x) (- height (*r k x))) data))
      (control-points (get-control-points xs scaled-data))
      )
    (g
      (path
        d (if smooth
              (str
                (format "M~a,~a " (first xs) (first scaled-data))
                (format "C ~a,~a ~a,~a ~a,~a "
                  (first xs) (first scaled-data)
                  (first (first control-points)) (second (first control-points))
                  (second xs) (second scaled-data))
                (implode
                  (map
                    (λ (x y z) (format "S ~a,~a ~a,~a"
                                  (first z) (second z)
                                  x y))
                    (drop xs 2)
                    (drop scaled-data 2)
                    (cdr control-points))
                  " "))
              (str
                (format "M~a,~a " (first xs) (first scaled-data))
                (implode
                  (map
                    (λ (x y) (format "L ~a,~a" x y))
                    (cdr xs)
                    (cdr scaled-data))
                  " ")))
        style (format "stroke-width: ~a; stroke: ~a" stroke-width color))
      (when dots
        (g
          (text
            (@
              'x (last xs)
              'y (- (last scaled-data) 20)
              'text-anchor "end"
              'style (format "fill: ~a; font-weight: bold" color))
            label)
          (for/fold
            ((s ""))
            ((x xs) (y scaled-data))
            (str
              s
              (circle
                cx x
                cy y
                r (+ stroke-width 1)
                class "plot_dot"
                style
                  (format
                    "stroke: ~a; opacity: 1; fill: white; stroke-width: ~a" ; write all style parameters to style attribute, as mf Illustrator doesn't read style tag with classes well
                    color
                    stroke-width)))))))))

; (linechart #:data '((1 210 340) (2 34 570) (3 80 14)))
(define (linechart
            #:data (data #f)
            #:datafile (datafile #f)
            #:headers (headers #t) ; are there headers ij csv?
            #:smooth (smooth #t)
            ; only separate style parameters:
            #:colors (colors '("black"))
            #:stroke-widths (stroke-widths '(2))
            ;
            #:index-seq (index-seq 1) ; which column in csv is x-axis indexes
            #:title (title "")
            #:x-axis (x-axis 1)
            #:y-axis (y-axis #f)
        )
  (let*
      ((data (if
                datafile
                (csv-file->list datafile #:headers headers)
                data))
      (headers (remove (first data) index-seq))
      (xs (map ->number (second data)))
      (charts (drop data 2))
      (x-axis (if (and x-axis (not (number? x-axis))) 1 x-axis))
      ; styles:
      (style-template "color: ~a; stroke-width: ~a")
      (styles (map-cycled
                (λ (plot color stroke-w)
                  (format style-template color stroke-w))
                charts colors stroke-widths))
      ; layout parameters:
      (TITLE_H TITLE_H)
      (Y-AXIS_W Y-AXIS_W)
      (PLOT_H PLOT_H)
      (PLOT_W PLOT_W)
      ; scaling:
      (k (get-scale-factor charts PLOT_H))
      (x0 (apply min xs))
      (kx (get-scale-factor xs PLOT_W #:base x0))
      (scaled-xs (map
                    (λ (x) (* kx (- x x0)))
                    xs))
      )
    (g
      (when title
        (text
          (@ 'class "title" 'x (h-centrify PLOT_W title) 'y (/r TITLE_H 2))
          title))
      (when y-axis
        (g (@ 'transform (svg/translate 0 TITLE_H))
          (for/fold
            ((s ""))
            ((yval y-axis))
            (let ((y (- PLOT_H (*r k yval))))
              (str
                s
                (text (@ 'x (- Y-AXIS_W 23) 'y y 'text-anchor "end") yval)
                (line x1 (- Y-AXIS_W 20) y1 y x2 (- Y-AXIS_W 10) y2 y))))))
      (when x-axis
        (g (@ 'transform (svg/translate Y-AXIS_W (+ TITLE_H PLOT_H)))
          (for/fold/idx
            (s "")
            (xval xs)
            (let ((x (*r kx (- xval x0))))
              (if (= (% $idx x-axis) 0)
                (str
                  s
                  (text (@ 'x x 'y (/r X-AXIS_H 2)) xval))
                s)))))
      (g (@ 'transform (svg/translate Y-AXIS_W TITLE_H))
          (for/fold/idx
            (s "")
            (chart charts)
            (str
              s
              (linechart-single
                scaled-xs
                chart
                #:smooth smooth
                #:color (nth-cycled colors (inc $idx))
                #:stroke-width (nth-cycled stroke-widths (inc $idx))
                #:dots #t
                #:label (nth headers (inc $idx))
                #:k k)))))))
