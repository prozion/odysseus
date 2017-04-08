#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")

(provide eventline)

(define-namespace-anchor a)

(define (get-label-name k)
  (let* ((subst-table (@ "&" "&amp;"))
        (txt (if (list? k) (first k) (str k))))
    (for/fold
      ((s txt))
      ((i (hash-keys subst-table)))
      (re-substitute s i (hash-ref subst-table i)))))

(define (eventline
          #:title (title #f)
          #:data (data #f)
          #:events-order (events-order #f)
          #:rate-lambda (rate-lambda #f)
          #:count-limit (count-limit 1)
          #:top (top #f)
          #:gap (gap 1)
          #:legend-y-font-size (legend-y-font-size 10)
          #:legend-y-w (legend-y-w 200)
          #:legend-x-font-size (legend-x-font-size 10)
          #:legend-x-rotate (legend-x-rotate 0)
          #:legend-x-events (legend-x-events #f)
          #:font-family (font-family "Verdana")
          #:vertical-ruler-color (vertical-ruler-color "#000")
          #:H (H PLOT_H)
          #:W (W PLOT_W)
          #:fill (fill "black")
          #:passive-fill (passive-fill #f) ; fill after the last event (usually grey)
          #:zebra-background (zebra-background (list "#fff" "#f0f0f0")))
  (define (make-rate-lambda rl h)
    (λ (a b)
      (rl (hash-ref h a) (hash-ref h b))))
  (let* (
          (title-h (if title TITLE_H 0))
          (data (if (path? data) (read-data-from-file data a) data))
          (events (if events-order
                        events-order
                        (sort
                          (uniques (flatten (hash-values data)))
                          (λ (a b) (< (->number a -0.1) (->number b -0.1))))))
          (max-event (last events))
          (events-number (length events))
          ;(_ (write-file "authors.txt" (implode (uniques (flatten (hash-keys data))) "\n" )))
          (data (for/fold
                  ((res data))
                  (((k v) data))
                  (if (< (length v) count-limit)
                    (hash-delete res k)
                    res)))
          (items (if rate-lambda
                      (sort (hash-keys data) (make-rate-lambda rate-lambda data))
                      (hash-keys data))) ;; STX sort
          (items (if top (lshift items top) items))
          (items-number (length items))
          (legend-x-h 20)
          (event-w (* 1.0 (/ (- W (* gap events-number) legend-y-w) events-number)))
          (track-h (* 1.0 (/ (- H (* gap items-number) legend-x-h title-h) items-number)))
          (track-w (- W legend-y-w))
          (zebra-background (if (> 2 (length zebra-background)) (list (first zebra-background) (first zebra-background)) zebra-background))
        )
    (str
      (when title
              (g (@ 'transform (svg/translate legend-y-w 0))
                (text
                  (@  'x (+ legend-y-w (h-centrify (- W legend-y-w) title)) ; (/ (+ legend-y-w W) 2.0) ;(h-centrify (- W legend-x-w) title)
                      'y 20
                      'text-anchor "middle"
                      'style (format "font-family: ~a; font-weight: bold; font-size: ~a" font-family (+ 3 legend-y-font-size)))
                  title)))
      (g (@ 'transform (svg/translate 0 (+ title-h legend-x-h)))
        (for/fold/idx
          (s "")
          (k items)
          (let ((track-list (hash-ref data k))
                (y (* (+ track-h gap) $idx)))
            (str
              s
              ; zebra background
              (rect 'x legend-y-w 'y y 'height track-h 'width track-w
                    'style (format "stroke: none; fill: ~a" (if (even? $idx) (first zebra-background) (second zebra-background))))
              ; bars of event participation
              (for/fold
                ((s2 ""))
                ((i track-list))
                (let* ( (xstart (+ legend-y-w (* (+ event-w gap) (dec (indexof events i)))))
                        (xend (+ xstart event-w gap)))
                  (str
                    (when (and passive-fill (equal? (first track-list) i))
                      (rect
                        'x legend-y-w
                        'y y
                        'width (- xstart legend-y-w)
                        'height track-h
                        'style (format "fill: ~a" passive-fill)))
                    s2
                    (rect
                      'x xstart
                      'y y
                      'width event-w
                      'height track-h
                      'style (format "fill: ~a" fill))
                    (when (and passive-fill (equal? (last track-list) i))
                      (rect
                        'x xend
                        'y y
                        'width (- W xend)
                        'height track-h
                        'style (format "fill: ~a" passive-fill)))
              )))
              ; label-y
              (text
                (@  'x (- legend-y-w 3)
                    'y (+ y -1 (v-centrify track-h #:font-size legend-y-font-size))
                    'text-anchor "end"
                    'style (format "font-family: ~a; font-weight: bold; font-size: ~a" font-family legend-y-font-size))
                (str
                  (get-label-name k)
                  " "
                  (format "<tspan style=\"~a\">~a</tspan>"
                              "fill: #aaa; font-weight: normal"
                              (length track-list))))
                ))))
      ; legend-x
      (g (@ 'transform (svg/translate legend-y-w title-h))
        (for/fold/idx
          (s "")
          (k events)
          (let ((x (* $idx (+ event-w gap)))
                (y (- legend-x-h 5)))
            (str
              s
              (line 'x1 x 'y1 legend-x-h 'x2 x 'y2 (- H title-h) 'style (format "stroke: ~a; opacity: 0.1" vertical-ruler-color))
              (when (or (not legend-x-events) (indexof? legend-x-events k))
                (let* ((xt (+ x (h-centrify event-w (str k) #:font-family font-family)))
                      (xtc (/ (text-length (str k)) 2))
                      (y (- y (if (notznil? legend-x-rotate) 5 0)))
                      (y2 (+ H (if (notznil? legend-x-rotate) 20 0) (- title-h) legend-x-font-size))
                      (x2 (- xt (if (notznil? legend-x-rotate) (/ xtc 2) 0))))
                  (str
                    (text (@
                            'x (+ xt (if (notnil? legend-x-rotate) xtc 0))
                            'y y
                            'text-anchor "middle"
                            'transform (svg/rotate legend-x-rotate (+ xt xtc) y)
                            'style (format "font-family: ~a; font-size: ~a" font-family legend-x-font-size))
                      (str k))
                    (text (@
                            'x x2
                            'y y2
                            'transform (svg/rotate legend-x-rotate x2 y2)
                            'style (format "font-family: ~a; font-size: ~a" font-family legend-x-font-size))
                      (str k)))))
            ))))
    )))
