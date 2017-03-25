#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")

(provide network2)

(define L0 50) ; optimal distance between connected nodes, probably around (/r (avg PLOT_W PLOT_H) (sqrt graph-size))))
(define MIN_DISTANCE 10) ; minimal distance between two nodes

(define (init-graph data)
  (for/fold
    ((res (hash)))
    (((k v) data))
      (hash-union
        (hash-union res (hash k (hash 'links v 'x 0 'y 0)))
        (for/hash ((i v)) (values i (hash 'links empty 'x 0 'y 0))))))

(define (build-graph graph x0 y0)


(define (network2
          #:data data
          #:labels (labels #f))
  (let* ( (x0 (/ PLOT_W 2))
          (y0 (/ PLOT_H 2))
          (graph (build-graph (init-graph data) x0 y0)))
    (for/fold
      ((res "")
      (((k v) graph))
      (let ((x (@. v.x))
            (y (@. v.y)))
        (str
          res
          (if labels
            (text
              (@
                'style (format "font-size:~a" 9)
                'x x
                'y (- y 3))
              k)
            "")
          ...))))))
