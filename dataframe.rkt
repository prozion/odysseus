#lang racket

(require "base.rkt")
(require "type.rkt")
(require "seqs.rkt")
(require "hash.rkt")
(require "io.rkt")
(require compatibility/defmacro)
(require (for-syntax "strings.rkt" racket/match))

(provide (all-defined-out))

(define-catch (dataframe columns (rows #f) #:hash-rows (h-rows #f))
  (let* ((columns columns)
        (rows (or h-rows
                  (for/fold
                      ((res empty))
                      ((row rows))
                      (pushr
                        res
                        (apply hash (interleave columns row)))))))
    (λ (f)
      (f columns rows))))

(define (columns df)
  (df (λ (cols _) cols)))

(define (rows df)
  (df (λ (_ rows) rows)))

(define (data df)
  (df (λ (columns rows)
        (for/fold
          ((res empty))
          ((h rows))
          (pushr
              res
              (map
                (λ (k) (hash-ref h k))
                columns))))))

(define-macro (loc df . args)
  (let-values
    (((row-exp col-exp)
        (match args
          (`(,row-exp ,col-exp) (values row-exp col-exp))
          (`(,col-exp) (values #f col-exp)))))
      `(let*
          ((rows-filter-lambda
            (cond
              ((equal*? ,row-exp ":") identity)
              ((not ,row-exp) identity)
              (else ,row-exp)))
          (cols-map-lambda
            (cond
              ((equal*? ,col-exp ":") identity)
              ((or
                (string? ,col-exp)
                (symbol? ,col-exp))
                  (λ (row) (hash-ref* row ,col-exp)))
              ((list? ,col-exp) (λ (row) (hash-delete-all row (minus (hash-keys row) ,col-exp))))
              (else ,col-exp)))
          (result-rows (filter rows-filter-lambda (rows ,df)))
          (result (map cols-map-lambda result-rows))
          (result-columns (if (not-empty? result)
                              (intersect (columns df) (hash-keys (first result)))
                              (intersect (columns df) (hash-keys (first (filter cols-map-lambda (rows ,df))))))))
            (dataframe result-columns #:hash-rows result-rows))))

(module+ test

  (require rackunit)

  (let* ((df (dataframe
                '(A B C D)
                '((1 2 3 4)
                  (5 6 7 8)
                  (9 10 11 12)))))

    (check-equal?
      (data (loc df ":" '(B C)))
      (data (dataframe
        '(B C)
        '((2 3)
          (6 7)
          (10 11)))))

    (check-equal?
      (data (loc
                df
                (λ (row) (> (apply + (hash-values row)) 10))
                '(A B D)))
      (data (dataframe
        '(A B D)
        '((5 6 8)
          (9 10 12)))))))
