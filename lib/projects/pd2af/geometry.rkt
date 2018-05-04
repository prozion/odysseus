#lang racket

(require compatibility/defmacro)
(require "../../load/all.rkt")
(require "types.rkt")
(require "common.rkt")
; (require "../../../../odysseus/lib/load/all.rkt")

(provide (all-defined-out))

; layout constants
(define W ($ el-w defaults))
(define H ($ el-h defaults))
(define uoiW ($ uoi-w defaults))
(define uoiH ($ uoi-h defaults))
(define gateW ($ gate-w defaults))
(define gateH ($ gate-h defaults))
(define gate-margin-0 ($ gate-margin defaults))
(define processW ($ process-w defaults))
(define processH ($ process-h defaults))

(define-catch (mutual-disposition ax1 ay1 ax2 ay2 bx1 by1 bx2 by2)
  (cond
    ((> ay1 by2)
      (cond
        ((< ax2 bx1) 'ne)
        ((> ax1 bx2) 'nw)
        (else 'n)))
    ((< ay2 by1)
      (cond
        ((< ax2 bx1) 'se)
        ((> ax1 bx2) 'sw)
        (else 's)))
    (else
      (cond
        ((< ax2 bx1) 'e)
        ((> ax1 bx2) 'w)
        ; after case when line starts at the top of IRF1 on N018:
        ((< (abs (- ax1 bx1)) (abs (- ax2 bx2))) 'w)
        ; (else 'overlap)
        (else 'e)))))

(define-catch (diagonal-quadrant x1 y1 x2 y2)
  (cond
    ((and (> x2 x1) (> (- x2 x1) (abs (- y2 y1))))
      'e)
    ((and (< x2 x1) (> (- x1 x2) (abs (- y2 y1))))
      'w)
    ((and (> y1 y2) (> (- y1 y2) (abs (- x2 x1))))
      'n)
    (else 's)))

(define-catch (c x1 x2 y1 y2)
  (list (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)))

(define-catch (mass-center elements)
  (let ((n (* 1.0 (length elements))))
    (let loop ((xc 0) (yc 0) (elements elements))
      (cond
        ((empty? elements) (hash 'x (/ xc n) 'y (/ yc n)))
        (else
          (let* (
                (e (car elements))
                (x (->number ($ x e)))
                (y (->number ($ y e)))
                (w (->number ($ w e)))
                (h (->number ($ h e)))
                (exc (+ x (/ w 2.0)))
                (eyc (+ y (/ h 2.0))))
            (loop (+ xc exc) (+ yc eyc) (cdr elements))))))))

(define-catch (get-node-xy-in-the-middle elements)
  (let* (
  			(coors-center-xy (mass-center elements))
  			(xc ($ x coors-center-xy))
  			(yc ($ y coors-center-xy))
  			(x (- xc (/ gateW 2.0)))
  			(y (- yc (/ gateH 2.0))))
    (hash 'x x 'y y)))

(define-catch (centrify el-w el-h box-x box-y box-w box-h)
  (hash
    'x (+ box-x (/ box-w 2.0) (/ el-w -2.0))
    'y (+ box-y (/ box-h 2.0) (/ el-h -2.0))))

(define-catch (calculate-arc-coors x1 y1 w1 h1 x2 y2 w2 h2 (d #f))
  (and x1 y1 w1 h1 x2 y2 w2 h2
    (let* (
          (x1 (->number x1)) (y1 (->number y1)) (w1 (->number w1)) (h1 (->number h1)) (x2 (->number x2)) (y2 (->number y2)) (w2 (->number w2)) (h2 (->number h2))
          (mutual-disp (mutual-disposition x1 y1 (+ x1 w1) (+ y1 h1) x2 y2 (+ x2 w2) (+ y2 h2)))
          (arc-coors
            (case mutual-disp
              ((n ne nw overlap) `(,@(c x1 (+ x1 w1) y1 y1) ,@(c x2 (+ x2 w2) (+ y2 h2) (+ y2 h2))))   ; top-center -> bottom-center
              ((e) `(,@(c (+ x1 w1) (+ x1 w1) y1 (+ y1 h1)) ,@(c x2 x2 y2 (+ y2 h2))))   ; right-center -> left-center
              ((w) `(,@(c x1 x1 y1 (+ y1 h1)) ,@(c (+ x2 w2) (+ x2 w2) y2 (+ y2 h2))))   ; left-center -> right-center
              ((s se sw) `(,@(c x1 (+ x1 w1) (+ y1 h1) (+ y1 h1)) ,@(c x2 (+ x2 w2) y2 y2)))   ; bottom-center -> top-center
            )))
      (hash 'x1 (first arc-coors) 'y1 (second arc-coors) 'x2 (third arc-coors) 'y2 (fourth arc-coors)))))

(define-catch (calculate-port-coors sources x y (gate-margin gate-margin-0) #:w (w gateW) #:h (h gateH))
  (let* (
        (average-source-coors (mass-center sources))
        (average-source-x ($ x average-source-coors))
        (average-source-y ($ y average-source-coors))
        (source-disposition (diagonal-quadrant x y average-source-x average-source-y))
        (result
          (case source-disposition
            ((n)
              (hash
                    'port-in-x (+ x (/ w 2.0))
        				    'port-in-y (- y gate-margin)
        				    'port-out-x (+ x (/ w 2.0))
        				    'port-out-y (+ y h gate-margin)))
            ((e)
              (hash
                    'port-in-x (+ x w gate-margin)
        				    'port-in-y (+ y (/ h 2.0))
        				    'port-out-x (- x gate-margin)
        				    'port-out-y (+ y (/ h 2.0))))
            ((w)
              (hash
        				    'port-in-x (- x gate-margin)
        				    'port-in-y (+ y (/ h 2.0))
                    'port-out-x (+ x w gate-margin)
                    'port-out-y (+ y (/ h 2.0))))
            ((s)
              (hash
        				    'port-in-x (+ x (/ w 2.0))
        				    'port-in-y (+ y h gate-margin)
                    'port-out-x (+ x (/ w 2.0))
                    'port-out-y (- y gate-margin))))))
    result))

(define-catch (get-line-between source target)
    (cond
      ((and (ActivityLogicalOperator? source) (ActivityLogicalOperator? target))
        (hash 'x1 ($ out-x source) 'y1 ($ out-y source) 'x2 ($ in-x target) 'y2 ($ in-y target)))
      ((and (ActivityLogicalOperator? source) (ActivityNode? target))
        (let* ((coors (calculate-arc-coors
                      ($ out-x source) ($ out-y source) 1 1
                      ($ x target) ($ y target) ($ w target) ($ h target)))
                (x2 ($ x2 coors))
                (y2 ($ y2 coors))
                )
          (hash 'x1 ($ out-x source) 'y1 ($ out-y source) 'x2 x2 'y2 y2)))
      ((and (ActivityNode? source) (ActivityLogicalOperator? target))
        (let* ((coors (calculate-arc-coors
                        ($ x source) ($ y source) ($ w source) ($ h source)
                        ($ in-x target) ($ in-y target) 1 1))
                (x1 ($ x1 coors))
                (y1 ($ y1 coors))
                )
          (hash 'x1 x1 'y1 y1 'x2 ($ in-x target) 'y2 ($ in-y target))))
      ((and (ActivityNode? source) (ActivityNode? target)
          (calculate-arc-coors
            ($ x source) ($ y source) ($ w source) ($ h source)
            ($ x target) ($ y target) ($ w target) ($ h target))))
      (else #f)))
