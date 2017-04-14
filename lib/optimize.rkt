#lang racket

(require compatibility/defmacro)
(require "seqs.rkt")
(require "hash.rkt")

(provide (all-defined-out))

(define (memoize f)
  (let ((args-hash (hash)))
    (λ args
      (let* ((hashed (hash-ref args-hash args #f))
            (res (or hashed (apply f args))))
        (when (not hashed) (set! args-hash (hash-insert args-hash (cons args res))))
        res))))

(define (opt/uniques lst)
  (for/fold
    ((res empty))
    ((i lst))
    (if (member i res)
      res
      (rpush res i))))

(define (opt/uniques/unordered lst)
  (hash-keys
    (make-hash (map (λ (x) (cons x #t)) lst))))
