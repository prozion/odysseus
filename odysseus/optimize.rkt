#lang racket

(require (rename-in  racket/hash  (hash-union hash-union-racket)))
(require compatibility/defmacro)
(require "base.rkt")
(require "list.rkt")
(require "hash.rkt")

(provide (all-defined-out))

(define (memoize f)
  (let ((cache (hash)))
    (λ args
      (let* ((cached-res (hash-ref cache args #f)) ; get cached value
            (res (or cached-res (apply f args))))
        (when (not cached-res) (set! cache (hash-set cache args res))) ; if value was not cached - cache it
        res))))

(define (opt/uniques lst)
  (for/fold
    ((res empty))
    ((i lst))
    (if (member i res)
      res
      (pushr res i))))
