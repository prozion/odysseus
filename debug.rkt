#lang racket

(require compatibility/defmacro)
(require (for-syntax "strings.rkt" racket/match))

(provide (all-defined-out))

(define debug-output (make-parameter #f))

(define debug
  (lambda args (apply string-append
    (map
      (lambda (el)
        (cond
          ((number? el) (number->string el))
          ((list? el) (list->string el)) ; list of chars to string
          (else el)))
      args))))

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

(define (print-with-delay p)
  (if p (begin (sleep (first p)) (flush-output) (display (second p))) (void)))

(define-syntax (--- stx)
  (syntax-case stx ()
    ((_ parameters ...)
      (with-syntax ((frmt #'(for/fold
                              ((s "~n"))
                              ((i (reverse (list parameters ...))))
                              (string-append "~a " s))))
        #'(printf frmt parameters ...)))))
        ; #'(if (debug-output)
        ;     (append-file (debug-output) (format frmt parameters ...))
        ;     (printf frmt parameters ...)
        ;     )))))

;;; shorthands
(define (print-list lst)
	(for ((i lst))
    (if (debug-output)
      (error "print-list now doesn't work with (debug-output) turned-on")
      ; (append-file (debug-output) i)
	    (println i))))

(define (---- obj)
  (cond
    ((list? obj) (print-list obj))
    ((hash? obj) (print-list (for/list (((k v) obj)) (cons k v))))
    (else obj)))

(define-macro (terpri)
	`(--- ""))

(define (----- n)
  (for ((i (range n))) (displayln ""))
  (flush-output))

(define-macro (let*-print . let-exprs)
  (let* ((let-forms (car let-exprs))
        (let-forms (for/fold
                      ((res '((_ (--- 1)))))
                      ((let-form let-forms) (i (in-naturals 2)))
                      (append res (list let-form) `((_ (--- ,i))))))
        (body-forms (cdr let-exprs)))
    `(let* ,let-forms ,@body-forms)))
