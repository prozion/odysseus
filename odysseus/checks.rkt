#lang racket

(require compatibility/defmacro rackunit)
(require "base.rkt")
(require "list.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "files.rkt")

(provide (all-defined-out))

(define-catch (check-hash h1 h2 #:list-any-order? (list-any-order? #f) #:values-any-order? (values-any-order? #f))
  (let* ((k1s (hash-keys h1))
        (k2s (hash-keys h2))
        (v1s (hash-values h1))
        (v2s (hash-values h2)))
    (cond
      (values-any-order?
        (for/and
          ((k1 k1s))
          (and
            (index-of? k2s k1 same-elements?)
            (let* ((position (index-where k2s (curry same-elements? k1)))
                  (k2 (list-ref k2s position)))
              (equal? (hash-ref h1 k1) (hash-ref h2 k2))))))
      (list-any-order?
        (and (same-elements? k1s k2s) (same-elements? v1s v2s))) ; in the case we don't care about elements order in the lists. Although doesn't work for cross-permutations yet
      (else
        (and
          (same-elements? k1s k2s) ; no extra unchecked keys neither at k1s nor at k2s
          (for/and
            ((k1 k1s))
            (equal? (hash-ref h1 k1 #f) (hash-ref h2 k1 #f))))))))

(define-macro (check-hash-equal? h1 h2)
  `(check-true
      (check-hash ,h1 ,h2)))

(define SPEED-UNIT (benchmark (apply / (range 1 2000)))) ; ~15 ms

(define-macro (check-speed expr high-limit)
  `(if (> (benchmark ,expr) (* ,high-limit SPEED-UNIT))
    (printf "~a - running time exceeds ~a speed units~n" ',expr ,high-limit)
    (void)))
