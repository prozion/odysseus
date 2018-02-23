#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/load/all.rkt")
(require "../../scrap/csv.rkt")
(require "../layout_globals.rkt")
(require "network.rkt")

(provide network2)

(define L0 50) ; optimal distance between connected nodes, probably around (/r (avg PLOT_W PLOT_H) (sqrt graph-size))))
(define L0-p (make-parameter L0))
(define MIN_DISTANCE 30) ; minimal distance between two nodes
(define MIN_DISTANCE-p (make-parameter MIN_DISTANCE))
(define X0 (/ PLOT_W 2))
(define Y0 (/ PLOT_H 2))

(define (get-reverse-links graph key)
  (for/fold
    ((res (list)))
    (((k v) graph))
    (if (indexof? v key)
      (pushr res k)
      res)))

(define (init-graph data #:directed (directed #f))
  (for/fold
    ((res (hash)))
    (((k v) data))
    (let* ( (links v)
            (links-ext (if (not directed)
                          (uniques (merge links (get-reverse-links data k)))
                          links)))
    ;(display k) (flush-output)
    (hash-union
      res
      (hash
        k
        (hash
          'links links-ext
          'x 0
          'y 0))
      (for/hash
        ((i links))
        (values
          i
          (hash 'links empty 'x 0 'y 0)))))))

(define (set-coors graph node x0 y0)
  (let* ((v (hash-ref graph node))
        (x (@. v.x))
        (y (@. v.y))
        (not-calculated (and (= 0 x) (= 0 y)))
        (x (if not-calculated x0 x))
        (y (if not-calculated y0 y))
        (res
    (hash-substitute graph (cons node (hash-substitute v (list (cons 'x x) (cons 'y y)))))))
    ;(println node)
    ;(when not-calculated (printf "~a~n~a~n~n" (hash-ref graph node) (hash-ref res node)))
    res))

(define (all-coors-established? graph)
  (not
    (ormap
      (λ (v) (and (= 0 (@. v.x)) (= 0 (@. v.y))))
      (hash-values graph))))

(define (find-possible-coors graph x0 y0 descendants-number)
  (define (find-possible-coors-next phi L count)
    (let* ( (x1 (+ x0 (* L (cos phi))))
            (y1 (- y0 (* L (sin phi))))
            (phi_delta (grad->rad (/ 360 descendants-number))))
    (cond
      ((> count descendants-number) (find-possible-coors-next phi (* 1.2 L) 0))
      ((or (not (< 5 x1 PLOT_W)) (not (< 10 y1 PLOT_H))) (find-possible-coors-next (+ phi phi_delta) L (inc count)))
      ((far-enough? graph x1 y1) (list x1 y1))
      (else
        (find-possible-coors-next (+ phi phi_delta) L (inc count))))))
  (let* ((phi0 (grad->rad (random 360))))
    (find-possible-coors-next phi0 (L0-p) 0)))

(define (far-enough? graph x y)
  (let ((established-coors
          (clean
            (λ (p) (= 0 (first p) (second p)))
            (map
              (λ (v) (list (@. v.x) (@. v.y)))
              (hash-values graph)))))
    (andmap
      (λ (p) (> (distance x y (first p) (second p)) (MIN_DISTANCE-p)))
      established-coors)))

(define (build-graph graph)
  (let ((acclst (list)))
    (define (build-graph-iter graph node x y)
      (printf "build-graph-iter: ~a ~a~n" (length (hash-keys graph)) (length acclst))
      (let* ((v (hash-ref graph node))
            (neighbours (@. v.links))
            (graph (cond
                      ((indexof? acclst node) graph) ; already calculated node, do nothing, just go to its neighbours
                      (else
                        (set! acclst (pushr acclst node)) ; <node> is not calculated yet, add <node> to acclst
                        (set-coors graph node x y))))) ; and set coors for <node>, returning new graph with positioned <node>
            (for/fold
              ((res (set-coors graph node x y))) ; change coordinates for current node, so we pass already updated graph into the next build-graph-iter
              ((n neighbours))
              #:break (all-coors-established? res) ; if all coordinates in graph are calculated - end current iteration
              (cond
                ((indexof? acclst n) res)
                (else
                  (let* ( (n-coors (find-possible-coors res x y (length neighbours)))
                          (x1 (first n-coors))
                          (y1 (second n-coors)))
                    (build-graph-iter
                      res
                      n ; for the next neighbouring node:
                      x1 ; change x
                      y1))))))) ; and change y
    (build-graph-iter graph (car (hash-keys graph)) X0 Y0)))

(define-namespace-anchor a)

(define (network2
          #:data data
          #:labels (labels #f)
          #:l0 (l0 L0)
          #:min-distance (min-distance MIN_DISTANCE))
  (let* (
          (data (if (path? data) (read-data-from-file data a) data))
          (graph (parameterize ((L0-p l0)
                                (MIN_DISTANCE-p min-distance))
                    (build-graph (init-graph data)))))
    ;(println (check-proximity graph))
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
                'x (+ x 1)
                'y (- y 3))
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
                (line
                  x1 x
                  y1 y
                  x2 (@. neighbour-v.x)
                  y2 (@. neighbour-v.y)
                  style (format "stroke:~a; opacity: 0.1" "black")))))
     )))))

; functions to check and debug
;; mutual distances in the graph:
(define (check-proximity graph)
  (let ((coors
          (map
            (λ (v)
              (list (@. v.x) (@. v.y)))
            (hash-values graph))))
    (filter (λ (r) (< r (MIN_DISTANCE-p)))
      (for/fold
        ((res (list)))
        ((a coors)
          #:when #t
          (b coors))
        (pushr res (distance (first a) (second a) (first b) (second b)))))))
