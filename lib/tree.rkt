#lang racket

(require "base.rkt")
(require "type.rkt")
(require "seqs.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "strings.rkt")
(require "regexp.rkt")

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
                (hash-union el (hash '_path path))
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

(define (same-elements? as bs (criterium? equal?))
  (cond
    ((and (scalar? as) (scalar? bs)) (criterium? as bs))
    ((and (list? as) (list? bs))
      (and
        (for/and
          ((a as))
          (ormap (λ (b) (same-elements? a b)) bs)))
        (for/and
          ((b bs))
          (ormap (λ (a) (same-elements? b a)) as)))
    (else (criterium? as bs))))

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
