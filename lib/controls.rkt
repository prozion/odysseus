#lang racket

(require compatibility/defmacro)
(require "base.rkt")

(provide (all-defined-out))

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

; (define (foo n) (nlet foo-iter (x y) (cond ... (else (foo-iter (add1 x) (sub1 y))))) (foo-iter 0 n))
;(define-macro (nlet n letargs . body)
;  `(define (,n ,@letargs)
;      ,@body))

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

(define-macro ($ name . args)
  `(define ,name (list ,@args)))
