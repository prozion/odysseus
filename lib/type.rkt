#lang racket

(require "seqs.rkt")
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
  (or (number? x) (string? x) (symbol? x)))

(define (sequence? x)
  (or (list? x) (hash? x) (vector? x)))

(define (not-empty-list? x)
  (and (list? x) (not (empty? x))))

(define (one-element? seq)
  (and (list? seq) (= (length seq) 1)))

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
                        (explode filtered)))
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
    (else (tostring x))))

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
  (indexof? '(number string) (type x)))

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

(module+ test

  (require rackunit)

  (check-pred scalar? 1)
  (check-pred scalar? 0.3)
  (check-pred scalar? "key")
  (check-pred scalar? 'key)
  (check-equal? (scalar? '()) #f)
  (check-equal? (scalar? (list 1)) #f)
  (check-equal? (scalar? (hash)) #f)
  (check-equal? (scalar? (hash 'a 1 'b 2)) #f)

  (check-true (sequence? '()))
  (check-true (sequence? '(1 2)))
  (check-true (sequence? (hash 'a 10)))
  (check-true (sequence? #(1 2 3)))
  (check-false (sequence? 5))
  (check-false (sequence? 'a))

  (check-true (not-empty-list? '(1)))
  (check-false (not-empty-list? '()))

  (check-true (one-element? '(1)))
  (check-true (one-element? '((1))))
  (check-false (one-element? '(1 2)))
  (check-false (one-element? '()))
  (check-false (one-element? 'a))
  (check-false (one-element? 10))

  (check-false (more-than-one-element? '(3)))
  (check-true (more-than-one-element? '(3 4)))
  (check-false (more-than-one-element? #f))

  (check-pred simple-cons? (cons 1 2))
  (check-equal? (simple-cons? (list 1 2)) #f)
  (check-equal? (simple-cons? (list 1)) #f)
  (check-equal? (simple-cons? (list)) #f)

  (check-pred cons-ext? (cons 1 2))
  (check-equal? (cons-ext? (list 1 2)) #t)
  (check-equal? (cons-ext? (list 1)) #t)
  (check-equal? (cons-ext? (list)) #f)

  (check-pred list2? '(()))
  (check-pred list2? '((1)))
  (check-pred list2? '((1 2 3 4)))
  (check-pred list2? '((1 2 3 4) (3 4 5) (8 9)))
  (check-pred list2? '(((9))))
  (check-pred list2? '((1 2 3 4) ((3 4 5) (8 9))))

  (check-false (list2? '((1 2 3 4) (3 4 5) 10 (8 9))))

  (check-true (plain-list? '(1 2 3 4 5)))
  (check-true (plain-list? '()))
  (check-true (plain-list? (list (hash 'a 10) (hash 'b 20 'c 30))))
  (check-true (plain-list? (list (hash (quote aa) 2) (hash (quote bb) 4 (quote cc) 8))))
  (check-false (plain-list? '(())))
  (check-false (plain-list? '(1 2 3 () 4 5)))
  (check-false (plain-list? '(1 2 3 (4 5) 4 5)))
  (check-false (plain-list? '(1 2 3 4 (5))))
  (check-false (plain-list? '(((1)) 2 3 4 5)))
  (check-false (plain-list? #f))
  (check-false (plain-list? (list (cons 1 2) (cons 1 3))))

  (check-pred list-of-simple-cons? (list (cons 1 2) (cons 4 5)))
  (check-equal? (list-of-simple-cons? (list (cons 1 2) (cons 4 5) 10)) #f)
  (check-equal? (list-of-simple-cons? '((1 2) (4 5))) #f)

  (check-equal? (type '()) 'list)
  (check-equal? (type 3) 'number)
  (check-equal? (type #t) 'boolean)
  (check-equal? (type "a str") 'string)
  (check-equal? (type #"a str") 'bytes)
  (check-equal? (type '((a 10) (b 20))) 'alist)
  (check-equal? (type (list (cons 1 2) (cons 3 4))) 'list-of-cons)
  (check-equal? (type '((a 10) (b 20 30))) 'list2)
  (check-equal? (type (list '(a 10) '(b 20 30) null)) 'list2)
  (check-equal? (type '((a 10) (b 20 30) 3)) 'list)
  (check-equal? (type '(1 2 3)) 'list)
  (check-equal? (type (cons 1 2)) 'pair)
  (check-equal? (type #\a) 'char)
  (check-equal? (type #\λ) 'char)
  (check-equal? (type #\u0011) 'char)
  (check-equal? (type 'a) 'symbol)
  (check-equal? (type (λ (x) x)) 'procedure)
  (check-equal? (type odd?) 'procedure)
  (check-equal? (type #'(+ 1 2)) 'syntax)
  (check-equal? (type #(1 2 3)) 'vector)
  (check-equal? (type (hash 'a 10 'b 20)) 'hash)
  (check-equal? (type (current-directory)) 'path)

  (check-equal? (->number 123) 123)
  (check-equal? (->number 123.789) 123.789)
  (check-equal? (->number "123a") 123)
  (check-equal? (->number "a123") 123)
  (check-= (->number "123.54") 123.54 1e-5)
  (check-= (->number "123,54") 123.54 1e-5)
  (check-= (->number "2 850,40") 2850.4 1e-5)
  (check-equal? (->number #f) #f)
  (check-equal? (->number "#f") #f)
  (check-equal? (->number "abc") #f)
  (check-equal? (->number "0") 0)
  (check-equal? (->number "00") 0)
  (check-equal? (->number "~10") 10)
  (check-equal? (->number ">10") 10)
  (check-equal? (->number "<10") 10)

  (check-equal? (->string "00") "00")
  (check-equal? (->string 100) "100")
  (check-equal? (->string 'a) "a")
  (check-equal? (->string '()) "")
  (check-equal? (->string '(a b c)) "abc")

  (check-equal? (->int "3.5") 3)
  (check-equal? (->int "00") 0)
  (check-equal? (->int 3.5) 3)
  (check-equal? (->int "3,5") 3)
  (check-equal? (->int "3") 3)
  (check-equal? (->int "2 850,40") 2850)

  (check-true (atom? 3))
  (check-true (atom? "abcdef"))
  (check-false (atom? (cons 1 2)))

  (check-pred alist? '((a 10) (b 20)))
  (check-pred alist? '((a 10)))
  (check-pred alist? '(((1 2) 10)))
  (check-pred alist? '((null null)))
  (check-false (alist? 3))
  (check-false (alist? 'a))
  (check-false (alist? '(3)))
  (check-false (alist? '(3 4)))
  (check-false (alist? '((a 10) (b 2 3))))

  (check-true (clist? '((1 . 2) (3 . 4))))
  (check-false (clist? '((1  2) (3  4))))
  (check-false (clist? '((1 . 2) (3  4))))

  (check-true (untype-equal? 3 3))
  (check-true (untype-equal? 3 (- 4 1)))
  (check-true (untype-equal? 3 "3"))
  (check-true (untype-equal? 'a "a"))
  (check-false (untype-equal? 'a 'b))
  (check-false (untype-equal? 'a 'ab))
  (check-false (untype-equal? "hello" "world"))
)
