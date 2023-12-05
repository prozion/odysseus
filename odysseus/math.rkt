#lang racket

(require "base.rkt")
(require "controls.rkt")
(require "list.rkt")
(require "string.rkt")
(require "type.rkt")
(require "debug.rkt")

(require compatibility/defmacro)

(provide (all-defined-out))

(define PI 3.1415926535897932)
(define π PI)

(define (avg . args)
  (/ (apply + args) (length args)))

(define (favg . args)
  (/f (apply + args) (length args)))

(define (gavg . args)
  (/ (apply * args) (length args)))

(define (grad->rad x) (* PI (/ x 180))) ; or use degrees->radians

(define (rad->grad x) (* 180 (/ x PI))) ; or use radians->degrees

; "a2" -> 162
(define (hex->dec v)
  (string->number (str "#x" v)))

; 162 -> "a2"
(define (dec->hex v)
  (define (next-hex v)
    (let ((hex "0123456789abcdef"))
      (string-ref hex (% v 16))))
  (define (dec->hex-r v)
    (cond
      ((= v 0) "")
      (else (str (dec->hex-r (quotient v 16)) (next-hex (% v 16))))))
  (cond
    ((= v 0) "0")
    (else (dec->hex-r v))))

(define (accumulate lst #:op (op +))
  (cond
    ((null? (cdr lst)) lst)
    (else
      (foldl
        (λ (x x0)
          (pushr x0 (op (last x0) x)))
        (list (car lst))
        (cdr lst)))))

(define (lg a x)
  (/ (log x) (log a)))

(define (distance x1 y1 x2 y2)
  (sqrt (+
          (sqr (- x2 x1))
          (sqr (- y2 y1)))))

; combinatorics
(define-catch (f! n)
  (cond
    ((or (equal? n 0) (equal? n 1)) 1)
    ((> n 1) (* n (f! (- n 1))))
    (else (errorf "Wrong number for factorial: ~a" n))))

(define (a-kn k n)
  (/
    (f! n)
    (f! (- n k))))

(define (c-kn k n)
  (/
    (f! n)
    (f! (- n k))
    (f! k)))

(define-poly (median args)
  (let* ((sorted (sort args <))
        (l (length args))
        (i1 (quotient l 2))
        (i2 (inc i1)))
    (if (even? l)
      (avg (nth sorted i1) (nth sorted i2))
      (nth sorted i2))))

(define-poly (mode args)
  (let* ((freqs
          (map
            (λ (x) (list x (length (filter (curry = x) args))))
            args))
        (freqs (uniques freqs))
        (freqs (sort freqs (λ (a b) (> (second a) (second b))))))
    (caar freqs)))

; (: make-frequency-hash : (-> (Listof T) (Immutable-HashTable T Integer)))
(define (make-frequency-hash seq)
  (for/fold
    ((res (hash)))
    ((el seq))
    (hash-set res el (inc (hash-ref res el 0)))))
