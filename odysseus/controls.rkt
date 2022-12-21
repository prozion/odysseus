#lang racket

(require compatibility/defmacro)
(require racket/stxparam)
(require racket/syntax (for-syntax racket/syntax racket/list))
(require "base.rkt")

(provide (except-out (all-defined-out) while-fun))

(define-macro (-> var . fs)
  (define (->rec var . fs)
    (cond
      ((empty? fs) var)
      (else
        (let* ((f (car fs))
              (new-expression
                (cond
                  ((list? f)
                    (append
                      (list (car f) var)
                      (cdr f)))
                  (else
                    (list f var)))))
          (apply ->rec (cons new-expression (cdr fs)))))))
  (apply ->rec (cons var fs)))

(define-macro (->> var . fs)
  (define (->>rec var . fs)
    (cond
      ((empty? fs) var)
      (else
        (let* ((f (car fs))
              (new-expression
                (cond
                  ((list? f)
                    (append f (list var)))
                  (else
                    (list f var)))))
          (apply ->>rec (cons new-expression (cdr fs)))))))
  (apply ->>rec (cons var fs)))

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

(define-syntax (for/fold/idx stx)
  (syntax-case stx ()
    [(_ (s start-value) (i sequence) body)
      (datum->syntax stx
        `(for/fold
          ([,#'s ,#'start-value])
          ([,#'i ,#'sequence] [$idx (range (length ,#'sequence))])
          ,#'body))
    ]
    ;[(_ (s start-value) ((k v) sequence) body)
    ;  (datum->syntax stx
    ;    `(for/fold
    ;      ([,#'s ,#'start-value])
    ;      ([(,#'k ,#'v) ,#'sequence] [$idx (range (length (hash-keys ,#'sequence)))])
    ;      ,#'body))
    ;]
))

(module+ test

  (require rackunit)
  ; (require "math.rkt")

  ; write avg anew, otherwise cycle loading conflict with math.rkt, where (avg ...) is originally defined
  (define (avg . args) (/ (apply + args) (length args)))

  (check-= ((--> sin sqrt) 4) (sin 2) 1e-6)

  (check-equal? ((~>
                    (λ (x y) (+ x y))
                    (λ args (list (length args) (apply + args))))
                  1 2 3 4 5)
                20)
  (check-equal? ((~>
                    (λ args (append args (list 10 20 30)))
                    (λ args (list (length args) (apply + args))))
                  1 2 3 4 5)
                '(5 15 10 20 30))
  (check-= ((~> sin avg) 10 2 45 6) 0 0.1)
  (check-= (apply (~> sin avg) '(10 2 45 6)) 0 0.1)

  (check-= (-->> sin sqrt 4) (sin 2) 1e-6)
  (check-equal? (-->> (λ (x) (filter odd? x)) (λ (x) (map (curry * 3) x)) (λ (x) (append '(-1 -2 -3) x)) '(1 2 3 4 5))
                '(-3 -9 3 9 15))

  (check-equal? (-> 100) 100)
  (check-equal?
    (-> "foo-bar" (string-replace "-" "/") string-upcase (string-split "/"))
    '("FOO" "BAR"))

  (check-equal? (gen 1 5) '(1 1 1 1 1))

  (let ((x 10))
    (check-equal? (the x 10 100) 100))
  (let ((x 10))
    (check-equal? (the x 8 100) 0))

  (let ((x 10))
    (check-equal? (thenot x 10 100) 0))
  (let ((x 10))
    (check-equal? (thenot x 8 100) 100))

  (check-equal? (ifthe (λ (x) (> x 5)) 9 sqr sqrt) 81)
  (check-equal? (ifthe (λ (x) (< x 5)) 9 sqr sqrt) 3)

  (check-equal? (repeat-f append '(1 2 3) '((4) (5 (6)) (7))) '(1 2 3 4 5 (6) 7))
  (check-equal? (repeat-f + 0 '(1 2 3)) 6)
  (check-equal? (repeat-f + 0 '()) 0)
)
