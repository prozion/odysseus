#lang racket

; (require "list.rkt")
(require "base.rkt")

(provide (all-defined-out))

(define (positive-number? x)
  (and (number? x) (> (->number x) 0)))

(define (non-negative-number? x)
  (and
    (number? (->number x))
    (>= (->number x) 0)))

(define (negative-number? x)
  (not (non-negative-number? x)))

(define (scalar? x)
  (or (number? x) (string? x) (symbol? x) (boolean? x)))

(define (true-sequence? x)
  (or (list? x) (hash? x) (vector? x)))

(define (one-element? seq)
  (and (list? seq) (= (length seq) 1)))

(define (two-elements? seq)
  (and (list? seq) (= (length seq) 2)))

(define single-element-list? one-element?)

(define (more-than-one-element? seq)
  (and (list? seq) (> (length seq) 1)))

(define several-elements? more-than-one-element?)

(define (simple-cons? x)
  (and
    (cons? x)
    (scalar? (car x))
    (scalar? (cdr x)) ))

(define (list2? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap list? x)))

(define (plain-list? x)
  (and
    (list? x)
    (not (ormap list? x))
    (not (ormap simple-cons? x))))

; some elements in list are lists
(define (list>=2? lst)
  (and
    (list? lst)
    (ormap (λ (x) (list? x)) lst)))

(define-catch (type x)
  (cond
    ((boolean? x) 'boolean)
    ((empty? x) 'list)
    ((number? x) 'number)
    ((string? x) 'string)
    ((bytes? x) 'bytes)
    ((char? x) 'char)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    ((syntax? x) 'syntax)
    ((vector? x) 'vector)
    ((hash? x) 'hash)
    ((path? x) 'path)
    ((void? x) 'void)
    ((simple-cons? x) 'simple-cons)
    ((alist? x) 'alist)
    ((clist? x) 'clist)
    ((list2? x) 'list2)
    ((list? x) 'list)
    (else #f)))

; delta is for special case met in sorting newspapers issues:
; to compare say 160 and 160a, we add small number (0.1) to the latter, so 160a comes after 160
(define (->number x (delta 0))
  (cond
    ((not x) #f)
    ((number? x) x)
    ((false? x) 0)
    ((and (string? x) (string->number x))
        (string->number x))
    ((equal? x "") 0)
    ((string? x)
      (let* ((filtered (string-replace x (regexp ",") "."))
            (filtered (filter
                        (λ (x) (or (string->number x) (equal? x ".")))
                        (filter non-empty-string? (string-split filtered ""))))
            (filtered (if (empty? filtered) #f (apply ~a filtered))))
        (and filtered (+ delta (string->number filtered)))))
    (else x)))

; ->number* transforming is used when working with dates
(define (->number* x (delta 0))
  (cond
    ((equal? x "xx") 1)
    ((equal? x "0x") 1)
    ((equal? x "1x") 10)
    ((equal? x "2x") 20)
    ((equal? x "3x") 30)
    (else (->number x delta))))

(define (->string x)
  (cond
    ((number? x) (number->string x))
    ((symbol? x) (symbol->string x))
    ((hash? x) (hash-pretty-string x))
    ((list? x) (apply string-append (map ->string x)))
    ((string? x) x)
    (else (~a x))))

(define (->int x)
  (and
    (number? (->number x))
    (int (->number x))))

(define (->symbol x (glue-char "_") #:transform-function (transform-function #f))
  (cond
    ((number? x) (string->symbol (number->string x)))
    ((and transform-function (string? x)) (string->symbol (transform-function x)))
    ((string? x) (string->symbol (string-replace x " " glue-char)))
    (else x)))

(define (->boolean x)
  (cond
    ((boolean? x) x)
    (else (if (equal? (->string x) "#f") #f #t))))

(define (atom? x)
  (member? (type x) '(number string)))

(define (alist? lst)
  (define (list-of-2? lst)
    (and (list? lst) (= (length lst) 2)))
  (cond
    ((not (list? lst)) #f)
    ((null? (cdr lst)) (list-of-2? (car lst)))
    (else (and (list-of-2? (car lst)) (alist? (cdr lst))))))

(define (clist? seq)
  (andmap simple-cons? seq))

(define (iso? a b)
  (printf "~a ~a ~a ~a~n" a b (type a) (type b))
  (equal? (type a) (type b)))

(define (equal*? x y)
  (equal? (->string x) (->string y)))

(define (listify x)
  (cond
    ((list? x) x)
    ((not x) empty)
    ((scalar? x) (list x))
    (else (list x))))

(define (scalarize x #:delimeter (delimeter " ") #:boolean-to-string (boolean-to-string #f))
  (cond
    ((list? x) (string-join (map scalarize x) delimeter))
    ((hash? x) (for/fold ((res "")) (((k v) x)) (format "~a~a~a ~a" res (if (equal? res "") "" delimeter) (scalarize x) (scalarize v))))
    ((boolean? x) (if boolean-to-string
                      (format "~a" x)
                      x))
    (else (format "~a" x))))

(define-catch (same-elements? as bs #:e (e? equal?))
  (cond
    ((and (scalar? as) (scalar? bs)) (e? as bs))
    ((and (list? as) (list? bs))
      (and
        (for/and
          ((a as))
          (ormap (λ (b) (same-elements? a b)) bs)))
        (for/and
          ((b bs))
          (ormap (λ (a) (same-elements? b a)) as)))
    (else (e? as bs))))

(define (iso-elements? as bs)
  (cond
    ((and (empty? as) (empty? bs)) #t)
    ((and (scalar? as) (scalar? bs)) (equal? (type as) (type bs)))
    ((and (list? as) (list? bs))
      (and
        (= (length as) (length bs))
        (iso-elements? (car as) (car bs))
        (iso-elements? (cdr as) (cdr bs))))
    (else #f)))
