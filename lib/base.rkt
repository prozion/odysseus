#lang racket

(require compatibility/defmacro)
(require (for-syntax racket))

(provide (all-defined-out))

(define nil null)

(define % remainder)

(define (int a)
  (inexact->exact (floor a)))

(define (fract a)
  (- a (int a)))

(define dec sub1)
(define inc add1)

(define (sqr a)
  (* a a))

(define-macro (make-rounded-op op round-op)
  `(λ xs (,round-op (apply ,op xs))))

(define // (make-rounded-op / exact->inexact))
(define /r (make-rounded-op / exact-round))
(define /f (make-rounded-op / exact-floor))
(define /c (make-rounded-op / exact-ceiling))
(define *r (make-rounded-op * exact-round))
(define *f (make-rounded-op * exact-floor))
(define *c (make-rounded-op * exact-ceiling))

(define (true? x) (if (equal? #f x) #f #t))

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (rand n)
  (add1 (random n)))

(define (nonempty? e) (not (empty? e)))
(define not-empty? nonempty?)

(define (not-equal? a b)
  (not (equal? a b)))

(define nil?
  (λ (v) (or
            (null? v)
            (void? v)
            (and (hash? v) (empty? (hash-keys v)))
            (and (string? v) (equal? v ""))
            (false? v))))

(define-macro (f-> f)
  `(λ preds
    (λ (argument)
      (let ((reslist (map (λ (x) (x argument)) preds)))
        (cond
          ((null? (cdr reslist)) (,f (car reslist)))
          (else
            (foldr
              (λ (a b) (,f a b))
              (car reslist)
              (cdr reslist)
           )))))))

(define and-> (f-> and))
(define or-> (f-> or))

(define-macro (op* o . args)
  (let* ((argument (last args))
        (ops (reverse (cdr (reverse args)))) ; without last element
        (ops (for/list ((op ops)) (list op argument))))
    `(,o ,@ops)))

(define-macro (and* . args)
  `(op* and ,@args))

(define-macro (or* . args)
  `(op* or ,@args))

(define (not-> f)
  (λ (argument)
    (not (f argument))))

(define notnil? (not-> nil?))

(define znil?
  (λ (v) (or
            (nil? v)
            (and
              (number? v)
              (= v 0)))))

(define notznil? (not-> znil?))

;; filtering
(define (clean f xs)
  ;(filter (λ (x) (not (f x))) xs))
  (filter-not f xs))

(define (in a b x)
  (<= a x b))

(define inii in)

(define (inee a b x)
  (< a x b))

(define (inei a b x)
  (and (< a x) (<= x b)))

(define (inie a b x)
  (and (<= a x) (< x b)))

(define-syntax (catch stx)
  (syntax-case stx ()
    ((_ place code ...)
      #'(with-handlers
          ((exn:fail? (λ (err) (printf "error in ~a: ~a~n" place (exn-message err)) (exit))))
          code ...))))

(define-syntax (try stx)
  (syntax-case stx (catch)
    ((_ code (catch result))
      #'(with-handlers
          ((exn:fail? (λ (err) result)))
          code))))

(define-syntax (define-catch stx)
  (syntax-case stx ()
    ((_ (name args ...) body ...)
      (with-syntax ((plain-name (datum->syntax stx (symbol->string (syntax->datum #'name)))))
        #'(define (name args ...)
            (catch
              plain-name
              body ...))))))


; has already been implemented in racket/syntax (format-symbol)
;(define-syntax (format-symbol stx)
;  (syntax-case stx ()
;    ((_ frmt syms ...) #'(string->symbol (format frmt syms ...)))
;    (else #'null)))

(define (symbol->list sym)
  (map
    string->symbol
    (filter
      non-empty-string?
      (string-split (symbol->string sym) ""))))

(define (listify a)
  (if (list? a) a (list a)))

(define (range+ start end)
  (cond
    ((< end start) empty)
    ((= end start) (list end))
    (else
      (append (range start end) (list end)))))

(define (dup value size)
  (map (λ (x) value) (range size)))

(define (but-last seq)
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
