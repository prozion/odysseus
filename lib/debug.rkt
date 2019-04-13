#lang racket

(require compatibility/defmacro)
(require (for-syntax "strings.rkt"))

(provide (all-defined-out))

(define debug-output (make-parameter #f))

(define-macro (benchmark . args)
  (let ((f (list-ref args 0))
        (descr (if (>= (length args) 2) (list-ref args 1) #f)))
    `(let* ((start-time (current-inexact-milliseconds))
            (res ,f)
            (post-time (- (current-inexact-milliseconds) start-time)))
        (if ,descr
          (begin
            (printf "~a ~a ms~n" ,descr post-time)
            res)
          post-time))))

(define-syntax (print-benchmark stx)
  (syntax-case stx ()
    ((_ args ...) #'(void (benchmark args ...)))))

(define-macro (show-status status-var text)
  `(when (,status-var)
    (display ,text)
    (flush-output)))

(define-macro (show-status-in-let status-var text)
  `(when (,status-var) (display ,text) (flush-output)))

(define-macro (_t text)
  `(show-status debug-output ,text))

(define-macro (__t text)
  `(show-status-in-let debug-output ,text))
