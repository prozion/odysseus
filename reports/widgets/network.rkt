#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")

(provide network)

(define epsilon 1) ; if all steps are less than epsilon - then stop moving nodes
(define L0 120) ; optimal distance between connected nodes, probably around (/r (avg PLOT_W PLOT_H) (sqrt graph-size))))
(define Ke 0.1) ; stifness of the graph edge at elongation
(define Ks (/ L0 2)) ; stifness of the graph edge at shrinkage
(define α 0.00001) ; repellion coefficient between not connected nodes
(define TELEPORT_R (* 0.1 L0)) ; distance to move node if it overlaps with another node
(define dt 1) ; time quant to calculate next positions of nodes
(define MAX_SPEED (* 0.5 L0)) ; maximal step of node movement per iteration
(define MAX_ITERATION_COUNT 300) ; maximal count for graph adjusting steps
(define MIN_DISTANCE 1) ; minimal distance between two nodes

(define (init-graph data)
  (for/fold
    ((res (hash)))
    (((k v) data))
    (let ((x (random PLOT_W))
          (y (random PLOT_H)))
      (hash-union
        (hash-union res (hash k (hash 'links v 'x x 'y y 'ux 0 'uy 0)))
        (for/hash ((i v)) (values i (hash 'links empty 'x x 'y y 'ux 0 'uy 0)))))))

(define (next-graph graph)
  (for/fold
    ((res (hash)))
    (((k v) graph))
    (let* ((x0 (@. v.x))
          (y0 (@. v.y))
          (links (@. v.links))
          (u (get-speed-delta graph k))
          ;(_ (println u))
          (dux (first u))
          (duy (second u))
          (ux (+ dux (@. v.ux)))
          ;(ux (if (> ux MAX_SPEED) MAX_SPEED ux))
          (uy (+ duy (@. v.uy)))
          ;(uy (if (> uy MAX_SPEED) MAX_SPEED uy))
          (x (+ x0 (* dt ux)))
          (y (+ y0 (* dt uy)))
          (x (cond
                ((< x 0) 0)
                ((> x PLOT_W) PLOT_W)
                (else x)))
          (y (cond
                ((< y 0) 0)
                ((> y PLOT_H) PLOT_H)
                (else y))))
      (hash-union res (hash k (hash 'links links 'x x 'y y 'ux ux 'uy uy))))))

(define (get-speed-delta graph node)
  (let* ( (v (hash-ref graph node))
          (x0 (@. v.x))
          (y0 (@. v.y))
          (links (@. v.links))
          (reverse-links (get-reverse-links graph node))
          (neighbours (merge links reverse-links))
          (valency (length neighbours)))
    ;(sleep 0.1)
    (for/fold
      ((res (list 0 0)))
      ((neighbour neighbours))
        (let* ((nv (hash-ref graph neighbour))
              (x (@. nv.x))
              (y (@. nv.y))
              (r (sqrt (+ (sqr (- x0 x)) (sqr (- y0 y)))))
              (dr (- r L0))
              (du (cond
                    ((> dr 0) (* Ke dr)) ; elongation
                    ((= r 0) MIN_DISTANCE) ; overlapping
                    ((< dr 0) (* Ks (/ dr L0))))) ; (/ dr r))))) ; shrinkage
              (phi (if (= x0 x)
                      (grad->rad 90)
                      (atan (abs (/ (- y0 y) (- x0 x))))))
              (dux (* (cos phi) du))
              (dux (/ dux valency))
              (dux (if (> x0 x) (- dux) dux))
              (duy (* (sin phi) du))
              (duy (/ duy valency))
              (duy (if (> y0 y) (- duy) duy)))
          ;(println (format "~a ~a" dr du))
          (list
            (+ (first res) dux)
            (+ (second res) duy))))))

(define (get-reverse-links graph key)
  (for/fold
    ((res (list)))
    (((k v) graph))
    (if (indexof? (@. v.links) key)
      (pushr res k)
      res)))

(define (mutual-repel graph #:steps (steps 1))
  (define (mutual-repel-iter count graph)
    (if (= count 0)
      graph
      (mutual-repel-iter
        (dec count)
        (for/fold
          ((res (hash)))
          (((k0 v0) graph))
          (let* ( (x0 (@. v0.x))
                  (y0 (@. v0.y))
                  (dr
                      (for/fold
                        ((res (list 0 0)))
                        (((k v) (hash-delete graph k0)))
                        (let ((repel-delta (get-repel-delta x0 y0 (@. v.x) (@. v.y))))
                          (list
                            (+ (first res) (first repel-delta))
                            (+ (second res) (second repel-delta))))))
                  (dx (first dr))
                  (dy (second dr))
                  (x (+ x0 dx))
                  (y (+ y0 dy)))
            (hash-union res (hash k0 (hash-substitute v0 (list (cons 'x x) (cons 'y y))))))))))
  (mutual-repel-iter steps graph))

(define (get-repel-delta x0 y0 x1 y1)
  (list
    (if (= x1 x0)
      TELEPORT_R
      (/ α (sgn (- x0 x1 1e-3)) (expt (- x0 x1) 2)))
    (if (= y1 y0)
      TELEPORT_R
      (/ α (sgn (- y1 y1 1e-3)) (expt (- y0 y1) 2)))))

(define (find-deltas old_graph new_graph)
  (for/fold
    ((res empty))
    (((k1 v1) old_graph))
    (let ((v2 (hash-ref new_graph k1)))
      ;(when (equal? k1 "DNA") (println (format "~a ~a" (@. v1.x) (@. v2.x))))
      (pushr
        res
        (sqrt
          (+
            (sqr (- (@. v2.x) (@. v1.x)))
            (sqr (- (@. v2.y) (@. v1.y)))))))))

(define (optimize-layout graph)
  (let ((graph-size (length (hash-keys graph))))
    (define (optimize-layout-iter graph deltas count)
      (let ((max_delta (apply max (map abs deltas))))
        ;(println max_delta)
        (cond
          ((< max_delta epsilon)
            (mutual-repel graph))
          ((> count MAX_ITERATION_COUNT)
            (mutual-repel graph))
          (else
            (let ((new_graph (next-graph graph)))
              (optimize-layout-iter new_graph (find-deltas graph new_graph) (inc count)))))))
      (optimize-layout-iter graph (gen (* 2 epsilon) graph-size) 0)))

(define (network
          #:data data
          #:labels (labels #f)
     )
  (let* ( (graph (init-graph data))
          (graph (optimize-layout graph)))
    (g (@ 'transform (svg/translate 10 30))
      (for/fold
        ((res ""))
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
            (for/fold
              ((s ""))
              ((neighbour (@. v.links)))
              (let
                ((neighbour-v (hash-ref graph neighbour)))
                  (if neighbour-v
                    (str
                      s
                      (line
                        x1 x
                        y1 y
                        x2 (@. neighbour-v.x)
                        y2 (@. neighbour-v.y)
                        style (format "stroke:~a; opacity: 0.3" "black")))
                    s)))
            (circle
              cx x
              cy y
              r 2
              style (format "fill:~a" "black"))))))))
