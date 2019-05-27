#lang racket

(require "base.rkt")
(require "type.rkt")
(require "seqs.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "strings.rkt")
(require "regexp.rkt")
(require "io.rkt")

(provide (all-defined-out))

(define (tree-filter f tr)
  (for/fold
    ((res (list)))
    ((t tr))
    (cond
      ((sequence? t) (pushr res (tree-filter f t)))
      ((f t) (pushr res t))
      (else res))))

(define (tree-clean next-level? exclude? tr)
  (for/fold
    ((res (list)))
    ((t tr))
    (cond
      ((next-level? t) (pushr res (tree-clean next-level? exclude? t)))
      ((exclude? t) res) ; in this cases f must handle also empty lists
      (else (pushr res t)))))

(define (tree-exclude tr el)
  (tree-clean
    list?
    (λ (x) (equal? x el))
    tr))

(define (hash-tree-flatten-with-paths htree (path (list)))
  (cond
    ((not (hash? htree))
      (cond
        ((list? htree)
          (map
            (λ (el)
              (if (hash? el)
                (hash-union (hash '_path path) el)
                el))
            htree))
        (else htree)))
    ((plain-hash? htree) (list (hash-union htree (hash '_path path))))
    (else
      (for/fold
        ((res (list)))
        ((key (hash-keys htree)))
        (append
          res
          (hash-tree-flatten-with-paths (hash-ref htree key) (pushr path key)))))))

(define (format-list pattern . inserts)
  (local ((define (format-list-iter head tail inserts)
            (cond
              ((empty? tail) (values head inserts))
              ((list? (car tail))
                (let-values (((el inserts) (format-list-iter (list) (car tail) inserts)))
                  (format-list-iter
                    (pushr head el)
                    (cdr tail)
                    inserts)))
              ((equal? (car tail) '~a)
                (format-list-iter
                  (if (equal? (car inserts) '$f)
                    head
                    (pushr head (car inserts)))
                  (cdr tail)
                  (cdr inserts)))
              ((equal? (car tail) '~@a)
                (format-list-iter
                  (if (equal? (car inserts) '$f)
                    head
                    (if (list? (car inserts))
                        (apply (curry pushr head) (car inserts))
                        (pushr head (car inserts))))
                  (cdr tail)
                  (cdr inserts)))
              ((equal? (car tail) '~s)
                (format-list-iter
                  (if (equal? (car inserts) '$f)
                    head
                    (pushr head (->string (car inserts))))
                  (cdr tail)
                  (cdr inserts)))
              ((equal? (car tail) '~@s)
                (format-list-iter
                  (if (equal? (car inserts) '$f)
                    head
                    (if (list? (car inserts))
                        (apply (curry pushr head) (map ->string (car inserts)))
                        (pushr head (->string (car inserts)))))
                  (cdr tail)
                  (cdr inserts)))
              (else
                (format-list-iter
                  (pushr head (car tail))
                  (cdr tail)
                  inserts)))))
    (let-values (((res extra-inserts)
                    (format-list-iter
                      (list)
                      pattern
                      inserts)))
      res)))

(define (transform-list-recur lst f)
  (cond
    ((scalar? lst) (f lst))
    ((plain-list? lst) (map f (f lst)))
    (else (map (λ (x) (transform-list-recur x f)) (f lst)))))

(define (same-elements? as bs #:e (e? equal?))
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

(module+ test

  (require rackunit)
  (require "type.rkt")
  (require "debug.rkt")

  (check-equal?
    (tree-filter
      (λ (el) (>= el 10))
      '(1 2 (3 (4 (5) 10 (11 3 12) (()) )) 80))
    '(((() 10 (11 12) (()))) 80))

  (check-equal?
    (tree-filter
      list?
      '(1 2 (3 (4 (5) 10 (11 3 12) (()) )) 80))
    '(((()()(())))))

  (check-equal?
    (tree-clean
      not-empty-list?
      empty?
      '(a (@ (id 10) ()) () (b (@ (id 20) (class "foo"))) () (c (@ (id 30)) (d (@ (id 40)) (e) ()))))
    '(a (@ (id 10)) (b (@ (id 20) (class "foo"))) (c (@ (id 30)) (d (@ (id 40)) (e)))))

  (check-equal?
    (tree-clean
      list?
      odd?
      '(1 2 3 () 4 5))
    '(2 () 4))

  (check-equal?
    (tree-exclude
      '(1 2 (3 4 ()) 5 () (6 (7 8 9) (7) ((7)) 7) 7 7 8)
      7)
    '(1 2 (3 4 ()) 5 () (6 (8 9) () (())) 8))

  (check-equal?
    (hash-tree-flatten-with-paths (hash 1 (hash 'a (list (hash 'aa 10) (hash 'ab 20)) 'b (hash 'bb 30))))
    (list
      (hash 'aa 10 '_path (list 1 'a))
      (hash 'ab 20 '_path (list 1 'a))
      (hash 'bb 30 '_path (list 1 'b))))

  (check-equal?
    (hash-tree-flatten-with-paths (hash 1 (hash 'a (list (hash 'aa 10) (hash 'ab 20)) 'b (hash 'bb 30)) 2 (hash 3 (hash 4 (hash 'c 70)))))
    (list
      (hash 'aa 10 '_path (list 1 'a))
      (hash 'ab 20 '_path (list 1 'a))
      (hash 'bb 30 '_path (list 1 'b))
      (hash 'c 70 '_path (list 2 3 4))))

  (check-equal?
    (hash-tree-flatten-with-paths
      (hash 1 (list (hash 'a 10))
            2 (list (hash 'b 20) (hash 'c 30))))
    (list
      (hash 'a 10 '_path (list 1))
      (hash 'b 20 '_path (list 2))
      (hash 'c 30 '_path (list 2))))

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

  (check-true (same-elements? 2 2))
  (check-false (same-elements? 2 3))
  (check-false (same-elements? '(1 2) '(2 3)))
  (check-true (same-elements? '(1 1 2) '(2 1)))
  (check-true (same-elements? '((2 1) (3 4)) '((1 2) (4 3))))
  (check-true (same-elements? '((2 (3 1 10)) (3 4)) '(((10 1 3) 2) (4 3))))
  (check-true (same-elements? '((2 (3 (1 2) 10)) (3 4)) '(((10 3 (2 1)) 2) (4 3))))
  (check-true (same-elements?
                '((((#f S1 (simple chemical)) (#f enzyme)) (#f P1 (simple chemical))) "positive influence")
                '((((#f enzyme) (#f S1 (simple chemical))) (P1 #f (simple chemical))) "positive influence")))

  (check-true (iso-elements? 2 2))
  (check-false (iso-elements? 2 "3"))
  (check-false (iso-elements? '(1 2) '(2 3 4)))
  (check-true (iso-elements? '(1 2) '(20 5)))
  (check-true (iso-elements? '(1 2 "foo") '(20 5 "3")))
  (check-false (iso-elements? '(1 "foo" 4) '(20 5 "3")))
)
