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

(define (true? x) (and x #t))

(define (!= a b)
  (not (= a b)))

(define (rcurry f a)
  (lambda (x) (f x a)))

(define (rand n)
  (add1 (random n)))

(define (nonempty? e) (and (list? e) (not (empty? e))))
(define not-empty? nonempty?)

(define (not-equal? a b)
  (not (equal? a b)))

(define-macro (when-not . expr)
  `(unless ,@expr))

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

; redirect define-catch to this one that will print each call to function for debug reasons:
; (define-syntax (define-catch stx)
;   (syntax-case stx ()
;     ((_ (function-name args ...) body ...)
;       (println (format "~a" #'function-name))
;       #'(define (function-name args ...) body ...))
;     ((_ var-name expr) #'(define var-name expr))))

; has already been implemented in racket/syntax (format-symbol)
;(define-syntax (format-symbol stx)
;  (syntax-case stx ()
;    ((_ frmt syms ...) #'(string->symbol (format frmt syms ...)))
;    (else #'null)))

(define not-empty-string? non-empty-string?)

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

(module+ test

  ; alist - harrison in Hurrycap castle
  ; base - personal guard in Emerald city
  ; controls - harrison in Gyngeme cave
  ; hash - harrison in Blinkers country
  ; interval - post near Miners cave
  ; math - harrison in the Cannibal castle
  ; seqs - army camp near Emerald city
  ; strings - post near Marrans country
  ; type - harrison in Chewers country
  ; regexp - circulating unit
  ; time - post in the mountains towards Gyngeme rocks and outward regiment
  ; optimize - post near Magic Forest
  ; bytes - outpost near Stella country
  ; stats - outpost near Villina country
  ; json - Great River fleet
  ; tree - Siege crue
  ; symbols - Engineering crue
  ; checks - Orchestra crue
  ; hashtree - Information and propaganda crue
  ; http - Kamikadze crue
  ; combinatorics - River fleet crue

  ; graphics/tests: spies
  ; color_test - spies and police in the Emerald city and around
  ; svg_test - spy troop in the surrounding mountains
  ; layout_test - spy troop across the plain land

  (require rackunit)

  (check-equal? (% 5 3) 2)

  (check-equal? (int 23.0) 23)
  (check-equal? (int 23.9) 23)

  (check-= (fract 23.0) 0 1e-9)
  (check-= (fract 23.9) 0.9 1e-9)

  (check-equal? (dec 5) 4)

  (check-equal? (inc 5) 6)

  (check-equal? (// 5 3) 1.6666666666666667)
  (check-= (// 5 3 1) 1.666666666666666 0.1)

  (check-equal? (/r 5 3) 2)
  (check-equal? (/r 5 3 1) 2)
  (check-equal? (/r 2 (sin 10)) -4)

  (check-equal? (/f 15 2 3) 2)

  (check-equal? (/c 15 2 3) 3)

  (check-equal? (*r 10 0.2 (sin 3) 80) 23)

  (check-equal? (*f (tan 1) (tan 2)) -4)
  (check-equal? (*f (tan 1) (tan -2)) 3)

  (check-equal? (*c (tan 1) (tan -2)) 4)

  (check-pred true? #t)
  (check-pred true? (+ 2 2))
  (check-pred true? (hash 'a 10))
  (check-pred true? 0)
  (check-pred true? null)
  (check-equal? (true? #f) #f)

  (check-true (nonempty? '(2)))
  (check-true (not-empty? '(2)))
  (check-false (nonempty? '()))
  (check-false (not-empty? '()))
  (check-false (nonempty? 3))
  (check-false (not-empty? 3))

  (check-pred nil? null)
  (check-pred nil? empty)
  (check-pred nil? '())
  (check-pred nil? (hash))
  (check-pred nil? (list))
  (check-pred nil? (cdr '(a)))
  (check-pred nil? "")
  (check-pred nil? #f)
  (check-pred nil? (unless #t #t))

  (check-equal? (filter (and-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(11 17))

  (check-equal? (filter (or-> odd? (λ (x) (> x 10))) '(1 2 3 11 12 14 17)) '(1 3 11 12 14 17))

  (check-equal? (filter (and->
                          odd?
                          (or->
                            (λ (x) (< x 10))
                            (λ (x) (> x 100))
                         ))
                        '(1 2 3 11 12 14 17 23 118 121 123))
                '(1 3 121 123))

  (check-equal? (filter (and->
                          odd?
                          (not-> (or->
                            (λ (x) (< x 10))
                            (λ (x) (> x 100))
                         )))
                        '(1 2 3 11 12 14 17 23 118 121 123))
                '(11 17 23))

  (check-true (and* number? odd? 3))
  (check-false (and* number? odd? 4))
  (check-false (and* number? odd? "a"))

  (check-true (or* number? odd? 3))
  (check-true (or* number? odd? 4))
  (check-false (or* number? symbol? "a"))

  (check-pred notnil? #t)
  (check-pred notnil? "txt")
  (check-pred notnil? 3)
  (check-pred notnil? 3.141592)

  (check-pred znil? 0)
  (check-pred znil? null)
  (check-pred znil? #f)
  (check-pred znil? "")
  (check-pred znil? '())
  (check-pred znil? (hash))

  (check-equal? (!= 1 0) #t)
  (check-equal? (!= 1 1) #f)

  (check-equal? (map (rcurry / 5) '(10 5 1)) '(2 1 1/5))

  (check-equal? (clean odd? '(1 2 3 4 5)) '(2 4))
  ;
  (check-true (andmap (λ (x) (inii 1 10 x)) (for/list ((_ (range 100))) (rand 10))))

  (check-equal? (in 0 1 0.5) #t)
  (check-equal? (in 0 1 2) #f)
  (check-equal? (in 0 1 0) #t)
  (check-equal? (in 0 1 1) #t)

  (check-equal? (inii -100 100 -100) #t)

  (check-equal? (inee 1 3 2) #t)
  (check-equal? (inee 0 1 0) #f)
  (check-equal? (inee 0 1 1) #f)

  (check-equal? (inei 1 3 2) #t)
  (check-equal? (inei 0 1 0) #f)
  (check-equal? (inei 0 1 1) #t)

  (check-equal? (inie 1 3 2) #t)
  (check-equal? (inie 0 1 0) #t)
  (check-equal? (inie 0 1 1) #f)

  (check-equal? (ok-or-false (first empty)) #f)
  (check-equal? (ok-or-false (second '(1))) #f)
  (check-equal? (ok-or-false (second '(1 2))) 2)
  (check-equal? (ok-or-false (third '(1 2 3 4 5))) 3)
  (check-equal? (ok-or-false (/ 1 0)) #f)
  (check-equal? (ok-or-false (sin 1 2)) #f)

  (check-equal? (symbol->list 'asdf) '(a s d f))
  (check-equal? (symbol->list 'a2) '(a |2|))
  (check-equal? (symbol->list 'a) '(a))
  (check-equal? (symbol->list 'foo) '(f o o))
  (check-equal? (symbol->list (string->symbol "")) '())

  (check-equal? (range+ 1 10) '(1 2 3 4 5 6 7 8 9 10))
  (check-equal? (range+ 3 3) '(3))
  (check-equal? (range+ 3 2) '())

  (check-equal? (dup 5 10) '(5 5 5 5 5 5 5 5 5 5))

  (check-equal? (but-last '(1 2 3)) '(1 2))
)
