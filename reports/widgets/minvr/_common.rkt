#lang racket

(provide (all-defined-out))

(define (find-place places p default-p)
  (let
      ((variants
        (filter
          (λ (x) (equal? (hash-ref x "name") p))
          (hash-values places))))
    (cond
      ((= (length variants) 0) default-p)
      ((= (length variants) 1) (car variants))
      (else (car variants)))))
