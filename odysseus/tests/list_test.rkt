#lang racket

(require "../list.rkt")

(require rackunit)

(check-true (not-empty-list? '(1)))
(check-false (not-empty-list? '()))

(check-equal? (implode '("a" "b" "c" "d") "-") "a-b-c-d")
(check-equal? (implode '("long" "sail" "across" "the" "sea") " ") "long sail across the sea")

(check-equal? (implode empty) "")
(check-equal? (implode empty "-") "")
(check-equal? (implode '(" " "b" "a" "k" "e" "\n" "r" "y")) " bake\nry")
(check-equal? (implode '(1 2 3 4)) "1234")
(check-equal? (implode '(1 2 3 4) "+") "1+2+3+4")
(check-equal? (implode '(1) "+") "1")
(check-equal? (implode (list null null null) ",") "(),(),()") ; дичь

(check-equal? (interleave '(1 2) '(3 4)) '(1 3 2 4))
(check-equal? (interleave '(1 2 10) '(3 4)) '(1 3 2 4))
(check-equal? (interleave '(1 2) '(3 4 10)) '(1 3 2 4))

(check-equal? (split-with (λ (x) (< x 5)) '(1 2 3 4 5 6 7 8 9 1 2 3)) '((1 2 3 4) (5 6 7 8 9 1 2 3)))
(check-equal? (split-with (λ (x) (< x 5)) '(1 2 3)) '((1 2 3) ()))
(check-equal? (split-with (λ (x) (< x 5)) '(9 10 5)) '(() (9 10 5)))
(check-equal? (split-with (λ (x) (< x 5)) '()) '(() ()))

(check-equal? (list-ref* '(0 1 2 3 4 5) 0) 0)
(check-equal? (list-ref* '(0 1 2 3 4 5) 2) 2)
(check-equal? (list-ref* '(0 1 2 3 4 5) 5) 5)
(check-equal? (list-ref* '(0 1 2 3 4 5) -1) 5)
(check-equal? (list-ref* '(0 1 2 3 4 5) -6) 0)
(check-equal? (list-ref* '(0 1 2 3 4 5) -10) #f)
(check-equal? (list-ref* '(0 1 2 3 4 5) 7) #f)

(check-equal? (nth '() 10) #f)
(check-equal? (nth '(0 1 2 3 4 5) 0) #f)
(check-equal? (nth '(0 1 2 3 4 5) 1) 0)
(check-equal? (nth '(0 1 2 3 4 5) 3) 2)
(check-equal? (nth '(0 1 2 3 4 5) -1) 5)
(check-equal? (nth '(0 1 2 3 4 5) -6) 0)
(check-equal? (nth '(0 1 2 3 4 5) 7) #f)

(check-equal? (indexof '(11 -22 30 80 -5) 30) 3)
(check-equal? (indexof '(11 -22 30 80 -5) -5) 5)
(check-equal? (indexof '(11 -22 30 80 -5) 333) 0)
(check-equal? (indexof '(11 -22 30 80 -5) -5 (λ (x y) (equal? x 30))) 3)

(check-false (index-of? '(a) 'd))
(check-true (index-of? '(a b c d e f) 'a))
(check-true (index-of? '(a b c d e f) 'd))
(check-true (index-of? '(1 (10 1) 2) '(10 1)))
(check-false (index-of? '(1 2 3 4 5) 6))

(check-equal? (count-element '(11 8 -22  8 30 80 -5 8) 8) 3)
(check-equal? (count-element '(1 1 1) 1) 3)

(check-true (regexp-index-of? '("doo" "fowl" "island") "doo"))
(check-true (regexp-index-of? '("doo" "fowl" "island") "i.*d"))
(check-true (regexp-index-of? '("doo" "fowl" "island") "do{2}"))
(check-true (regexp-index-of? '("doo" "fowl" "island") "f[oae]wl"))
(check-false (regexp-index-of? '("doo" "fowl" "island") "baz"))
(check-false (regexp-index-of? '("doo" "fowl" "island") "f[auy]+wl"))

(check-equal? (drop* '(1 2 3 4 5 6 7 8 9) 2) '(3 4 5 6 7 8 9))

(check-equal? (list-conj '() 100) '(100))
(check-equal? (list-conj '(1 2 3) 100) '(100 1 2 3))
(check-equal? (list-conj '((1 2 3)) '(100 200)) '((100 200) (1 2 3)))
(check-equal? (list-conj '(1 2) 3 4 5) '(5 4 3 1 2))

(check-equal? (take-right* '(1 2 3 4 5 6 7 8 9)) '(9))
(check-equal? (take-right* '(1 2 3 4 5 6 7 8 9) 2) '(8 9))

(check-equal? (pushr '() 100) '(100))
(check-equal? (pushr '(1 2 3) 100) '(1 2 3 100))
(check-equal? (pushr '(1 2 3) 3) '(1 2 3 3))
(check-equal? (pushr '(1 2 3) 3) '(1 2 3 3))
(check-equal? (pushr '(1) 2 3 4) '(1 2 3 4))
(check-equal? (pushr '(1) 2 '(3 4)) '(1 2 (3 4)))

(check-equal? (append-elements '(1 2 3) 4) '(1 2 3 4))
(check-equal? (append-elements '(1 2 3) 4 5) '(1 2 3 4 5))

(check-equal? (slice '() 1 3) null)
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 7) '(2 3 4 5 6 7))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 100) '(2 3 4 5 6 7 8 9))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 100) '(1 2 3 4 5 6 7 8 9))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 3) '(1 2 3))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -1) '(1 2 3 4 5 6 7 8 9))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -3) '(1 2 3 4 5 6 7))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) -4 -2) '(6 7 8))
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 2) '())
(check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 3) '(3))

(check-equal? (splice '(1 2 3 4 5 6) '(100 200) 3) '(1 2 3 100 200 4 5 6))

(check-equal? (not-uniques '(1 2 3 1 10 7 3 4 4)) '(1 3 4))
(check-equal? (not-uniques '(1 2 13 12 10 7 3 4 14)) '())

(check-equal? (uniques '(1 2 3 1 10 7 3 4 4)) '(1 2 3 10 7 4))
(check-equal? (uniques '(1 2 (3 4) 1 10 7 3 4 4 (3 4))) '(1 2 (3 4) 10 7 3 4))

(check-equal? (minus '() '()) '())
(check-equal? (minus '(1 2 3) '()) '(1 2 3))
(check-equal? (minus '() '(1 2 3)) '())
(check-equal? (minus '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3))
(check-equal? (minus '(1 2 3 4 5) '(2 1 3 5 4)) '())
(check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4))) '((1 2)))
(check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '())
(check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal?) '((1 2)))
(check-equal? (minus '((-1 0) (1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '((-1 0)))
(check-equal? (minus '((-1 0) 3 (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '((-1 0) 3))

(check-equal? (minus '((-1 0) 3 (3 4) ((10 20) (30 40))) '((2 1) (3 4) ((40 30) (20 10))) #:equal-f deep-equal-set?) '((-1 0) 3))

(check-equal? (intersect '() '()) '())
(check-equal? (intersect '(1 2 3) '()) '())
(check-equal? (intersect '() '(1 2 3)) '())
(check-equal? (intersect '(1 2 3 4 5) '(9 8 7 6 5 4)) '(4 5))
(check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6)) '(1 2 3))
(check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6) '(1 2 8)) '(1 2))
(check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6) '()) '())
(check-equal? (intersect '(1 2 3 5) #f) '())

(check-true (intersect? '(1 2 3 5) '(3 2 1 1 6)))
(check-false (intersect? '(1 2 3 5) '(4 8 10)))
(check-false (intersect? '(1 2 3 5) #f))
(check-false (intersect? '() '(3 2 1 1 6)))
(check-false (intersect? '(3 2 1 1 6) '()))
(check-false (intersect? '() '()))

(check-equal? (difference '() '()) '())
(check-equal? (difference '(1 2 3) '()) '(1 2 3))
(check-equal? (difference '() '(1 2 3)) '(1 2 3))
(check-equal? (difference '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3 9 8 7 6))
(check-equal? (difference '(1 2 3 5) '(3 2 1 1 6)) '(5 6))
(check-equal? (difference '(1 2 3 5) '(5 2 1 3)) '())
(check-equal? (difference '(1 1 1 1) '()) '(1 1 1 1))

(check-equal? (unique-difference '(1 1 1 1) '()) '(1))

(check-true (equal-elements? '() '()))
(check-equal? (equal-elements? '(#f) (list (< 3 2))) #t)
(check-true (equal-elements? '(1 1 1 1) '(1)))
(check-true (equal-elements? '(1 2 3) '(1 2 3)))
(check-true (equal-elements? '(1 2 3) '(3 2 1 1)))
(check-false (equal-elements? '(1 1 1 1) '(1 1 1 2)))
(check-false (equal-elements? '(1 1 1 1) '(2 2 2)))
(check-false (equal-elements? '(1 1 1 1) '()))

(check-true (equal-set? '(1 2 3) '(1 2 3)))
(check-true (equal-set? '(1 2 3) '(3 2 1)))
(check-true (equal-set? '(1 1 2 3) '(3 1 2 1)))
(check-true (equal-set? '() '()))
(check-true (equal-set? '(2) (list (- 3 1))))
(check-false (equal-set? '(1 2 3) '(3 2 1 1)))
(check-false (equal-set? '(1 2 3 3) '(3 2 1 1)))
(check-false (equal-set? '(1 2 3) '(1 2 3 4)))
(check-false (equal-set? '(1 2 3) '()))
(check-false (equal-set? '() '(1 2 3)))

(check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
(check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 4) '((1 2 3 4) (5 6 7 8) (9)))
(check-equal? (partition-all '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12) (13 14 15)))
(check-equal? (partition-all '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
(check-equal? (partition-all '(1 2 3 4 5) 0) '(1 2 3 4 5))

(check-equal? (flatten '((1 2 3) (4 5 6))) '(1 2 3 4 5 6))
(check-equal? (flatten '((1 (2 (3))) (4 ((5)) 6) 7)) '(1 2 3 4 5 6 7))
(check-equal? (flatten '(1 2 3)) '(1 2 3))

(check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))

(check-equal? (cleanmap '(1 2 3 #f 4 #f "a" '() 3)) '(1 2 3 4 "a" '() 3))

(check-equal? (append-unique '(1 2 3) '(4 5 1 6)) '(1 2 3 4 5 6))
(check-equal? (append-unique '(1 2 1 3) '(4 5 1 6 2 2 2)) '(1 2 3 4 5 6))
(check-equal? (append-unique '(1 1 3) '(4 5 1 6 2) '(2 2) '(7 3 8 8 (9) 10)) '(1 3 4 5 6 2 7 8 (9) 10))

(check-equal? (format-list '(a b ~a d) '(c)) '(a b (c) d))
(check-equal? (format-list '(a b ~a) 'c) '(a b c))
(check-equal? (format-list '(a b ~a d) 'c) '(a b c d))
(check-equal? (format-list '(a b ~a d e) 'c) '(a b c d e))
(check-equal? (format-list '(a b ~a d (e (f ~a))) '(c) 'u) '(a b (c) d (e (f u))))
(check-equal? (format-list '(a b ~a d) (for/list ((i (in-range 1 3))) i)) '(a b (1 2) d))
(check-equal? (format-list '(a b ~a d) (for/fold ((res (list))) ((i (in-range 1 3))) `(,@res ,i))) '(a b (1 2) d))
(check-equal? (format-list '(a b ~a d e ~a f g) 'c '$f) '(a b c d e f g))
(check-equal? (let ((x '$f)) (format-list '(a b ~a d e ~a f g) 'c x)) '(a b c d e f g))
(check-equal? (format-list '(a b ~a d e ~@a f g) 'c '(1 2)) '(a b c d e 1 2 f g))
(check-equal? (format-list '(a b ~a d e ~@a f g) 'c '((a 10) (b 20))) '(a b c d e (a 10) (b 20) f g))
(check-equal? (format-list '(a b ~a d e ~@a f g) 'c 3) '(a b c d e 3 f g))
(check-equal? (format-list '(a b ~a d e ~@a f g) 'c '$f) '(a b c d e f g))
(check-equal? (format-list '(a b ~a d e ~@a f g ~a l) 'c '$f 'k) '(a b c d e f g k l))
(check-equal? (format-list '(a b ~a d e ~s f g ~s l ~@s) 'a 'b 10 '(30 "40")) '(a b a d e "b" f g "10" l "30" "40"))
; (check-equal? (format-list '(~a ~@a) 'a '($f)) '(a))
; (check-equal? (format-list '(~a ~@a) 'a '(b $f c)) '(a b c))

(check-equal? (transform-list-recur
                '(1 2 3 (4 5) (6 (10 8) 7) () (8 10 3) (10 1 2) 9 10 11 (10 (3 4)) 12)
                (λ (x) (if (and (not-empty-list? x) (equal? (car x) 10))
                          `(,(* 2 (car x)) ,@(cdr x))
                          x)))
              '(1 2 3 (4 5) (6 (20 8) 7) () (8 10 3) (20 1 2) 9 10 11 (20 (3 4)) 12))
(check-equal? (transform-list-recur
                '(1 2 (3 (10 4 (10 5 6 (10 7) 8 (10 9 10) 11) 12 (10 (13))) 14) 15 (10 16))
                (λ (x) (if (and (not-empty-list? x) (equal? (car x) 10))
                          `(a ,@(cdr x))
                          x)))
              '(1 2 (3 (a 4 (a 5 6 (a 7) 8 (a 9 10) 11) 12 (a (13))) 14) 15 (a 16)))
(check-equal? (transform-list-recur
                '(10 2 (3 (10 4 (10 5 6 (10 7) 8 (10 9 10) 11) 12 (10 (13))) 14) 15 (10 16))
                (λ (x) (if (equal? x 10)
                          'a
                          x)))
              '(a 2 (3 (a 4 (a 5 6 (a 7) 8 (a 9 a) 11) 12 (a (13))) 14) 15 (a 16)))
