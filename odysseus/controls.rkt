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

(define-macro (some->> var . fs)
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
          (if new-expression
            (apply ->>rec (cons new-expression (cdr fs)))
            #f)))))
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
))
