#lang racket

(require compatibility/defmacro)
(require (for-syntax "strings.rkt" racket/match))

(provide (all-defined-out))

(define debug-output (make-parameter #f))

; (define-macro (benchmark . args)
;   (let ((f (list-ref args 0))
;         (descr (if (> (length args) 1) (list-ref args 1) #f)))
;     `(let* ((start-time (current-inexact-milliseconds))
;             (res ,f)
;             (post-time (- (current-inexact-milliseconds) start-time)))
;         (if ,descr
;           (begin
;             (printf "~a ~a ms~n" ,descr post-time)
;             res)
;           post-time))))

(define-macro (benchmark . args)
  (let-values (((descr args)
                  (match args
                    (`((d ,description-string) ,expr-args ...) (values description-string expr-args))
                    (`(,expr-args ...) (values #f expr-args)))))
      `(let* ((start-time (current-inexact-milliseconds))
              (res (begin ,@args))
              (post-time (- (current-inexact-milliseconds) start-time)))
          (printf "~a~a ms~n" (if ,descr (format "~a: " ,descr) "") post-time)
          res)))

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

(define-macro (errorf frmt . args)
	`(error (format ,frmt ,@args)))
