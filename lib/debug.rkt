#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

;(define t
;    (make-parameter
;      (if (parameter? t)
;        (t)
;        (current-inexact-milliseconds))))

;(define t
;  (let ((start 0))
;    (λ ()
;        (if (= start 0)
;          (begin
;            (set! start (current-milliseconds))
;            0)
;          (- (current-milliseconds) start)))))
;
;; print-run-time
;(define (prt (label "label"))
;  (printf "~a: ~a~n" label (t))
;  "")

(define-macro (benchmark . args)
  (let ((f (list-ref args 0))
        (descr (if (>= (length args) 2) (list-ref args 1) #f)))
    `(let* ((start-time (current-inexact-milliseconds))
            (res ,f)
            (post-time (- (current-inexact-milliseconds) start-time)))
        (if ,descr
          (begin
            (printf "~a~a~n" ,descr post-time)
            res)
          post-time))))
