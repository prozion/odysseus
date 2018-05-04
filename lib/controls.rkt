#lang racket

(require compatibility/defmacro)
(require racket/stxparam)
(require racket/syntax (for-syntax racket/syntax))
(require "base.rkt")

(provide (except-out (all-defined-out) while-fun))

; ((-> floor sqrt random) 10)
(define (-> . fs)
  (define (call-r fs x)
    (cond
      ((empty? fs) x)
      (else ((car fs) (call-r (cdr fs) x)))))
  (λ (x)
    (call-r fs x)))

; (->> floor sqrt random 10)
(define (->> . fs)
  (cond
    ((empty? (cdr fs)) (car fs))
    (else
      ((car fs) (apply ->> (cdr fs))))))

; (gen (random 100) 10) -> '(1 34 50 7 80 62 58 91 10 8)
(define-macro (gen f size)
  `(let ((n ,size))
    (define (gen-r count)
      (cond
        ((= count 1) (list ,f))
        (else (cons ,f (gen-r (- count 1))))))
    (gen-r n)))

(define set-id
  (let ((ids (list))
        (prefix ""))
    (λ (#:prefix (pref #f) #:reset (reset #f))
      (when reset
        (set! ids (list)))
      (when pref
        (set! prefix pref))
      (set! ids (cons (format-symbol "~a~a" prefix (length ids)) ids))
      (first ids))))

; (the y-axis-pos 'left y-axis-left) -> (if (equal? y-axis-pos 'left) y-axis-left 0)
(define-syntax (the stx)
  (syntax-case stx ()
    ([_ var (test ...) result]
      (syntax (if (ormap (curry equal? var) '(test ...))
                result
                0)))
    ([_ var test result]
      (syntax (if (equal? var test) result 0)))))

; (thenot y-axis-pos 'hidden 10) -> (if (not (equal? y-axis-pos 'hidden)) 10 0)
(define-syntax (thenot stx)
  (syntax-case stx ()
    ([_ var (test ...) result]
      (syntax (if (not (ormap (curry equal? var) '(test ...)))
                result
                0)))
    ([_ var test result]
      (syntax (if (not (equal? var test)) result 0)))))

(define-syntax (ifthe stx)
  (syntax-case stx ()
    ([_ testlambda expr iflambda ifnotlambda]
      #'(if (testlambda expr) (iflambda expr) (ifnotlambda expr)))
    ([_ testlambda expr iflambda]
      #'(begin (if (testlambda expr) (iflambda expr) #f)))
    ([_ testlambda expr]
      #'(if (testlambda expr) expr #f))))

(define (zor . body)
  (cond
    ((null? body) #t)
    ((null? (cdr body)) (car body))
    (else (if
            (or
              (nil? (car body))
              (equal? (car body) 0))
                (apply zor (cdr body))
                (car body)))))


; <name> will consume both inline arguments and arguments as list
; def -> (name arguments)
(define-macro (define-poly def . body)
  (let ((name (car def))
        (arguments-list-name (cadr def)))
  `(define (,name . args)
    (let ((lambda-args (λ ,arguments-list-name ,@body)))
      (cond
        ((null? args) (lambda-args))
        ((list? (car args)) (apply lambda-args (car args)))
        (else (apply lambda-args args)))))))

(define (random-word size #:prefix (prefix ""))
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
        (letters (map string (string->list letters))))
    (define (random-word-iter size result)
      (if (<= size 0)
        result
        (random-word-iter (dec size) (string-append result (list-ref letters (random (length letters)))))))
    (string-append prefix (random-word-iter size ""))))

(define (while-fun condition body)
  (when (condition)
    (body)
    (while-fun condition body)))

(define-syntax-rule (while condition body ...)
  (while-fun
      (lambda () condition)
      (lambda () body ...)))

(define (repeat-f f arg-acc arg-lst)
  (cond
    ((empty? arg-lst) arg-acc)
    (else (repeat-f f (f arg-acc (car arg-lst)) (cdr arg-lst)))))
