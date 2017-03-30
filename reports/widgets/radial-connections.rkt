#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../layout_globals.rkt")

(provide radial-connections)

(define (init-graph graph)
  (for/fold
    ((res graph))
    (((k v) graph))
    (hash-substitute res (cons k (hash 'x 0 'y 0 'links v)))))

(define (only-root-links graph)
  (λ (x) (indexof? (hash-keys graph) x)))

(define (reduce-graph graph)
  (for/fold
    ((res graph))
    (((k v) graph))
    (let ((filtered (filter (only-root-links graph) v)))
    (hash-substitute res (cons k (filter (only-root-links graph) v))))))

(define (distribute-along-circle graph diameter)
  (let* ((radius (/ diameter 2))
        (phi-step (/ 360 (length (hash-keys graph))))
        (x0 (/ PLOT_W 2))
        (y0 (/ PLOT_H 2)))
    (for/fold
      ((res graph))
      (((k v) graph)
        (phi (map grad->rad (range 0 360 phi-step))))
      (hash-substitute
        res
        (cons
          k
          (hash-substitute
            v
            (list
              (cons 'x (+ x0 (* radius (cos phi))))
              (cons 'y (- y0 (* radius (sin phi)))))))))))

(define-namespace-anchor a)

(define (radial-connections
          #:data data
          #:labels (labels #f)
          #:diameter (diameter PLOT_H)
          #:links-curveness (links-curveness 1))
  (let* (
          (data (if (path? data) (read-data-from-file data a) data))
          (data (reduce-graph data))
          (graph (init-graph data))
          (graph (distribute-along-circle graph diameter)))
    (g (@ 'transform (svg/translate 0 30))
      (for/fold
        ((s ""))
        (((k v) graph))
        (let ((x (@. v.x))
              (y (@. v.y))
              (links (@. v.links)))
          (str
            s
            ; text:
            (if (and labels (>= (length links) labels))
              (text
                (@
                  'style (format "font-family: Verdana; font-size:~a" 10)
                  'x (+ x
                        (if
                          (> x (/ PLOT_W 2))
                          1
                          (- (+ 3 (text-length (str k))))))
                  'y (+ y
                        (if
                          (< y (/ PLOT_H 2))
                          (- 3)
                          (+ 3 (text-height (str k))))))
                k)
              "")
            ; dot:
            (circle
              cx x
              cy y
              r 2
              style (format "fill:~a" "black"))
            ; lines:
            (for/fold
              ((s2 ""))
              ((link links))
              (let ((neighbour-v (hash-ref graph link)))
                (str
                  s2
                  ;(path
                  ;  d (str
                  ;      (format "M~a,~a " x y)
                  ;      (format "" ...))
                  ;  style (format "stroke:~a; opacity: 0.1" "black"))
                  (line
                    x1 x
                    y1 y
                    x2 (@. neighbour-v.x)
                    y2 (@. neighbour-v.y)
                    style (format "stroke:~a; opacity: 0.1" "black")))))
          ))))))
