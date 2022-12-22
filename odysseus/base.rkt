#lang racket

(require compatibility/defmacro)
(require (for-syntax racket))

(provide (all-defined-out))

(define nil null)

(define combine compose)

(define % remainder)

(define (int a)
  (inexact->exact (floor a)))

(define (fract a)
  (- a (int a)))

(define dec sub1)
(define inc add1)

(define-macro (make-rounded-op op round-op)
  `(λ xs (,round-op (apply ,op xs))))

(define // (make-rounded-op / exact->inexact))
(define /r (make-rounded-op / exact-round))
(define /f (make-rounded-op / exact-floor))
(define /c (make-rounded-op / exact-ceiling))
(define *r (make-rounded-op * exact-round))
(define *f (make-rounded-op * exact-floor))
(define *c (make-rounded-op * exact-ceiling))

(define (true? x) (and x #t))

(define (!= a b)
  (not (= a b)))

(define (rand n)
  (add1 (random n)))

(define not-empty? (negate empty?))

(define not-equal? (negate equal?))

(define-macro (if-not condition e1 e2)
  `(if ,condition ,e2 ,e1))

(define-macro (when-not . expr)
  `(unless ,@expr))

(define nil?
  (λ (v) (or
            (null? v)
            (void? v)
            (and (hash? v) (empty? (hash-keys v)))
            (and (string? v) (equal? v ""))
            (false? v))))

(define not-nil? (negate nil?))

(define-macro (op* o . args)
  (let* ((argument (last args))
        (ops (reverse (cdr (reverse args)))) ; without last element
        (ops (for/list ((op ops)) (list op argument))))
    `(,o ,@ops)))

(define-macro (and* . args)
  `(op* and ,@args))

(define-macro (or* . args)
  `(op* or ,@args))

(define clean filter-not)

(define-syntax (catch stx)
  (syntax-case stx ()
    ((_ place code ...)
      #'(with-handlers
          ((exn:fail? (λ (err)
                        (error (format "~a:~n~a" place (exn-message err))))))
                        ; (exit))))
          code ...))))

(define-syntax (try stx)
  (syntax-case stx (catch)
    ((_ code (catch result))
      #'(with-handlers
          ((exn:fail? (λ (err) result)))
          code))))

; (or-or-false (second '(1))) -> #f
(define-syntax (ok-or-false stx)
  (syntax-case stx ()
    ((_ body ...)
      #'(with-handlers
          ((exn:fail? (λ (err) #f)))
          body ...))))

(define-syntax (define-catch stx)
  (syntax-case stx ()
    ((_ (name args ...) body ...)
      (with-syntax ((plain-name (datum->syntax stx (symbol->string (syntax->datum #'name)))))
        #'(define (name args ...)
            (catch
              plain-name
              body ...))))))

(define (dup value size)
  (build-list size (const value)))

(define (butlast seq)
  (reverse (cdr (reverse seq))))

(define (hash-pretty-string h)
  (let* ((ks (hash-keys h))
        (res-str
          (for/fold
            ((res ""))
            ((k ks))
            (~a
              res
              (format "~a : ~a~n" k (hash-ref h k))))))
  res-str))
