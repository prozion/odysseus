#lang racket

(require "list.rkt")
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

(define (not-empty-list? x)
  (and (list? x) (not (empty? x))))

(define (one-element? seq)
  (and (list? seq) (= (length seq) 1)))

(define (two-elements? seq)
  (and (list? seq) (= (length seq) 2)))

(define single-element-list? one-element?)

(define (more-than-one-element? seq)
  (and (list? seq) (> (length seq) 1)))

(define several-elements? more-than-one-element?)

(define (simple-cons? x)
  (and (pair? x) (not (list? x))))

(define (cons-ext? x)
  (and (pair? x) (scalar? (car x))))

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

(define (list-of-cons? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap cons? x)))

(define (list-of-simple-cons? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap simple-cons? x)))

(define (list-of-seqs? x)
  (and
    (list? x)
    (not (empty? x))
    (andmap pair? x)))

; some elements in list are lists
(define (list>=2? lst)
  (and
    (list? lst)
    (ormap (λ (x) (list? x)) lst)))

(define (plain-hash? x)
  (and
    (hash? x)
    (not (ormap
            (λ (el) (cond
                      ((hash? el) #t)
                      ((list? el) (ormap hash? el))
                      (else #f)))
            (hash-values x)))))

(define (nested-hash? hh)
  (and
    (hash? hh)
    (ormap (λ (v) (hash? v)) (hash-values hh))))

(define-catch (type x)
  (cond
    ((boolean? x) 'boolean)
    ((empty? x) 'list)
    ((number? x) 'number)
    ((string? x) 'string)
    ((bytes? x) 'bytes)
    ((alist? x) 'alist)
    ((list-of-simple-cons? x) 'list-of-cons)
    ((list2? x) 'list2)
    ((list? x) 'list)
    ((pair? x) 'pair)
    ((char? x) 'char)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    ((syntax? x) 'syntax)
    ((vector? x) 'vector)
    ((hash? x) 'hash)
    ((path? x) 'path)
    ((void? x) 'void)
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
    ((string? x)
      (let* ((filtered (string-replace x (regexp ",") "."))
            (filtered (filter
                        (λ (x) (or (string->number x) (equal? x ".")))
                        (filter-not non-empty-string? (string-split filtered ""))))
            (filtered (if (empty? filtered) #f (implode filtered))))
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
  (index-of? '(number string) (type x)))

(define (alist? lst)
  (define (list-of-2? lst)
    (and (list? lst) (= (length lst) 2)))
  (cond
    ((not (list? lst)) #f)
    ((null? (cdr lst)) (list-of-2? (car lst)))
    (else (and (list-of-2? (car lst)) (alist? (cdr lst))))))

(define (clist? seq)
  (andmap simple-cons? seq))

(define (untype-equal? a1 a2)
  (equal? (->string a1) (->string a2)))

(define (iso? a b)
  (printf "~a ~a ~a ~a~n" a b (type a) (type b))
  (equal? (type a) (type b)))

(define (equal*? x y)
  (equal? (->string x) (->string y)))

(define (symbol-symbol? s)
  (and
    (not (symbol? s))
    (symbol? (eval s (make-base-namespace)))))

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
