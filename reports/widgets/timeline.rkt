#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")

(provide timeline)

(define-namespace-anchor a)

(define (timeline
          #:mind mind
          #:maxd maxd
          #:title (title #f)
          ;;
          #:data (data #f)
          #:datafile (csvfile #f)
          ;;
          #:timerange (timerange #f)
          ;; view
          #:font-family (font-family FONT-FAMILY)
          #:y-font-size (y-font-size FONT-SIZE)
          #:y-axis (y-axis #t)
          #:y-axis-w (y-axis-w Y-AXIS_W)
          #:x-font-size (x-font-size FONT-SIZE)
          #:x-axis-h (x-axis-h X-AXIS_H)
          #:x-rotate (x-rotate 0)
          #:x-ticks (x-ticks #f)
          #:gap (gap 1)
          #:H (H PLOT_H)
          #:W (W PLOT_W)
          #:fill (fill "black")
          #:zebra-background (zebra-background (list "#fff" "#f0f0f0"))
          #:tick-color (tick-color "black")
          #:tick-h (tick-h 5)
          ;;
          #:dot (dot #f)
        )
  (let* (
          (mind (date->days mind))
          (maxd (date->days maxd))
          (data (if (path? data) (read-data-from-file data a) data))
          (title-h (if title TITLE_H 0))
          (screen-h (- H title-h x-axis-h 0.0))
          (screen-w (- W y-axis-w 0.0))
          (tracks (hash-keys data))
          (track-h (/ screen-h (length tracks)))
          (y-axis-w (if y-axis y-axis-w SPAN))
          (x-ratio (/ screen-w (- maxd mind)))
          (date->x (λ (adate)
                          (let ((ds (date->days adate)))
                            (cond
                              ((< ds mind) 0)
                              ((> ds maxd) screen-w)
                              (else (* (- ds mind) x-ratio))))))
        )
    (str
      (when title
        (g (@ 'transform (svg/translate y-axis-w 0))
          (text
            (@  'x (h-centrify screen-w title)
                'y (/ title-h 2)
                'text-anchor "middle"
                'style (format "font-family: ~a; font-weight: bold; font-size: ~a" font-family (+ 3 y-font-size)))
            title)))
      (g (@ 'transform (svg/translate 0 title-h))
        (for/fold/idx
          (s "")
          (track tracks)
          (let* ((track-events (hash-ref data track))
                (dates (sort
                          (hash-keys track-events) d<))
                (y (* (+ track-h gap) $idx)))
            (printf "[widgets/timeline.rkt] dates number: ~a~n" (length dates))
            (str
              s
              (g (@ 'transform (svg/translate y-axis-w 0))
                ; zebra background
                (rect 'x 0 'y y 'height track-h 'width screen-w
                      'style (format "stroke: none; fill: ~a" (if (even? $idx) (first zebra-background) (second zebra-background))))
                ; bars/dots/lines
                (for/fold
                  ((s2 ""))
                  ((d dates))
                  (let* ((x1 (date->x (if (list? d) (car d) d)))
                        (x2 (if (list? d) (date->x (cdr d)) x1)))
                    (str
                      s2
                      (cond
                        ((!= x1 x2)
                                    (rect
                                      'x x1
                                      'y y
                                      'width (- x2 x1)
                                      'height track-h
                                      'style (format "fill: ~a" fill)))
                        (dot
                                    (circle
                                      'cx x1
                                      'cy (+ y (/ track-h 2))
                                      'r 5
                                      'style (format "fill: ~a" fill)))
                        (else
                                    (line
                                      'x1 x1
                                      'y1 y
                                      'x2 x1
                                      'y2 (+ y track-h)
                                      'style (format "stroke: ~a; opacity: 0.2" fill))))

              ))))
              ; label-y
              (when y-axis
                (text
                  (@  'x (- y-axis-w 3)
                      'y (+ y -1 (v-centrify track-h #:font-size y-font-size))
                      'text-anchor "end"
                      'style (format "font-family: ~a; font-weight: bold; font-size: ~a" font-family y-font-size))
                  track))
      ))))
      ; x
      (when x-ticks
        (g (@ 'transform (svg/translate y-axis-w (+ title-h screen-h)))
          (for/fold/idx
            (s "")
            (xdate x-ticks)
            (let* (
                  (major-tick (notznil? (nth xdate 3)))
                  (tick-y (* (if major-tick 3 1) tick-h))
                  (y (+ x-font-size tick-y))
                  (x (if (list? xdate) (first xdate) xdate))
                  (label (str (if (list? xdate) (second xdate) xdate)))
                  (x (date->x x))
                  )
              (str
                s
                ;(line 'x1 x 'y1 x-axis-h 'x2 x 'y2 (- H title-h) 'style (format "stroke: ~a; opacity: 0.1" tick-color))
                (line 'x1 x 'y1 0 'x2 x 'y2 tick-y 'style (format "stroke: ~a; opacity: 1" tick-color))
                (str
                  (text (@
                          'x x
                          'y y
                          'text-anchor "start"
                          ;'transform (svg/rotate x-rotate x2 y2)
                          'style (format "font-family: ~a; font-size: ~a; font-weight: ~a" font-family x-font-size (if major-tick "bold" "normal")))
                    label))
              )))))
    )))
