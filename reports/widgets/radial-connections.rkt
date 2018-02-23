#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../graphics/fonts.rkt")
(require "../../graphics/layout.rkt")
(require "../../lib/load/all.rkt")
(require "../layout_globals.rkt")

(provide radial-connections reduce-graph)

(define (init-graph graph)
  (for/fold
    ((res graph))
    (((k v) graph))
    (hash-substitute res (cons k (hash 'x 0 'y 0 'links v)))))

(define (only-root-links graph)
  (λ (x) (indexof? (hash-keys graph) x)))

(define (reduce-graph graph #:post-lambda (post-lambda #f))
  (let* ((acc (hash))
        (post-lambda2 (if post-lambda
                          (λ (x) (cond
                                    ((hash-ref acc x #f) (hash-ref acc x))
                                    (else
                                      (let ((res (post-lambda x)))
                                        (set! acc (hash-insert-fuse acc (cons x res)))
                                        res))))
                          #f))
        (result (for/fold
                  ((res graph))
                  (((k v) graph))
                  (let ((filtered (filter (only-root-links graph) v)))
                  (hash-substitute res (cons k (filter (only-root-links graph) v)))))))
    (if post-lambda2
          (for/fold
            ((r result))
            (((k v) result))
            (hash-insert-fuse
              (hash-delete r k)
                (cons (post-lambda2 k) (map post-lambda2 v))))
          result)))

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
          #:dots (dots #f)
          #:link-opacity (link-opacity 0.1)
          #:tick-length (tick-length 30)
          #:link-curveness (link-curveness 1)
          #:show-numbers (show-numbers #f)
          #:print (post-lambda #f)
          #:print-replace (print-replace #f))
  (let* (
          (x0 (/ PLOT_W 2))
          (y0 (/ PLOT_H 2))
          (data (if (path? data) (read-data-from-file data a) data))
          (data (reduce-graph data #:post-lambda post-lambda))
          (graph (init-graph data))
          (graph (distribute-along-circle graph diameter)))
    (g (@ 'transform (svg/translate 0 50))
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
              (let* (
                    (left? (> x x0))
                    (top? (< y y0))
                    (rl tick-length)
                    (xl1 (+ x (if left? 2 -2)))
                    (yl1 (+ y (if top? -2 2)))
                    ;(xl2 (+ x
                    ;      (if left? 30 -30)))
                    (xl2 (+ x (* rl (cos (get-angle (- x x0) (- y0 y))))))
                    ;(_ (println (* rl (cos (rad->grad (get-angle (- x x0) (- y0 y)))))))
                    ;(yl2 (+ y
                    ;      (if top? -30 30)))
                    (yl2 (- y (* rl (sin (get-angle (- x x0) (- y0 y))))))
                    (xt (+ xl2 (if left? 5 -5)))
                    (yt (+ yl2 (text-height (str k))))
                    (text-anchor (if left? "start" "end")))
                (str
                  (line x1 x y1 y x2 xt y2 yt style (format "opacity: ~a" 0.3))
                  (text
                    (@
                      'style (format "font-family: Verdana; font-size: ~a; text-anchor: ~a" 10 text-anchor)
                      'x xt
                      'y yt)
                    (let ((txt (if print-replace
                                    (replace-all (symbol->string k) (first print-replace) (second print-replace))
                                    k)))
                      (if show-numbers
                            (format "~a (<tspan style=\"font-weight: bold;\">~a</tspan>)"
                                    txt
                                    (length links))
                            txt)))))
              "")
            ; dot:
            (when dots
              (circle
                cx x
                cy y
                r dots
                style (format "fill:~a" "black")))
            ; lines:
            (for/fold
              ((s2 ""))
              ((link links))
              (let*-values (
                  ((neighbour-v) (hash-ref graph link))
                  ((tx ty) (values (@. neighbour-v.x) (@. neighbour-v.y)))
                  ((x1 y1 x2 y2) (if (< x tx) (values x y tx ty) (values tx ty x y)))
                  ((α1 α2) (values (get-angle (- x1 x0) (- y0 y1)) (get-angle (- x2 x0) (- y0 y2))))
                  ((dα) (abs (- (rad->grad α1) (rad->grad α2))))
                  ((dist) (distance x1 y1 x2 y2))
                  ;((rx) (* 2 dist (sqrt (/ dα 180))))
                  ;((rx) (* 2 dist))
                  ((rx) (* 1 (expt dist (+ link-curveness 0.2))))
                  ((ry) rx)
                  ((x-axis-rotation) 0)
                  ((large-arc-flag) 0) ; use a flatter arc
                  ((sweep-flag)
                    (let ((aq1 (angle-quadrant α1))
                          (aq2 (angle-quadrant α2)))
                    (cond
                      ; exception - angles in quadrants 1 and 2 are smaller than in 3 and 4, but actually bigger, so we have to force it to be 1 in the case when point in q3,4 is to the left of point in q1, and 0 when point in q1,2 is lefter than q4
                      ((and (or (= aq1 4)) (= aq2 1)) 1)
                      ((and (or (= aq1 1)) (= aq2 4)) 0)
                      (else
                        (if (> α2 α1)
                          (if (< dα 180) 1 0)
                          (if (< dα 180) 0 1)
                       ))))))
                (str
                  s2
                  (path
                    d (str
                        (format "M~a,~a " x1 y1)
                        (format "A ~a ~a ~a ~a ~a ~a ~a" rx ry x-axis-rotation large-arc-flag sweep-flag x2 y2))
                    style (format "stroke:~a; opacity: ~a" "black" link-opacity)))))
       ))))))
