#lang racket

(provide (all-defined-out))

;(define t
;    (make-parameter
;      (if (parameter? t)
;        (t)
;        (current-inexact-milliseconds))))

(define t
  (let ((start 0))
    (λ ()
        (if (= start 0)
          (begin
            (set! start (current-milliseconds))
            0)
          (- (current-milliseconds) start)))))

; print-run-time
(define (prt (label "label"))
  (printf "~a: ~a~n" label (t))
  "")
