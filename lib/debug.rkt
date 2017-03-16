#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define status-output (make-parameter #f))

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

(define-macro (show-status status-var text)
  `(when (,status-var)
    (display ,text)
    (flush-output)))

(define-macro (show-status-in-let status-var text)
  `(when (,status-var) (display ,text) (flush-output)))

(define-macro (_t text)
  `(show-status status-output ,text))

(define-macro (__t text)
  `(show-status-in-let status-output ,text))
