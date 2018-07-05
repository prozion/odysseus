#lang racket

(require compatibility/defmacro rackunit)
(require "base.rkt")
(require "seqs.rkt")
(require "tree.rkt")
(require "hash.rkt")
(require "debug.rkt")

(provide (all-defined-out))

(define (check-hash-equal/simple? h1 h2)
  (equal?
    (sort
      (hash-values h1)
      <)
    (sort
      (hash-values h2)
      <)))

(define (check-hash h1 h2 #:list-any-order? (list-any-order? #f))
  (let* ((k1s (hash-keys h1))
        (k2s (hash-keys h2))
        (v1s (hash-values h1))
        (v2s (hash-values h2)))
    (if list-any-order?
      (and (same-elements? k1s k2s) (same-elements? v1s v2s)) ; in the case we don't care about elements order in the lists. Although doesn't work for cross-permutations yet
      (and
        (same-elements? k1s k2s) ; no extra unchecked keys neither at k1s nor at k2s
        (for/and
          ((k1 k1s))
          (and
            (indexof? k2s k1)
            (equal? (hash-ref h1 k1) (hash-ref h2 (nth k2s (indexof k2s k1))))))))))

(define-macro (check-hash-equal? h1 h2)
  `(check-true
      (check-hash ,h1 ,h2)))

(define-syntax (check-hash-difference stx)
  (syntax-case stx ()
    ((_ h1 h2) #'(check-hash-difference-1 h1 h2 "1" "2"))
    ((_ h1 h2 h1-semantic h2-semantic) #'(check-hash-difference-1 h1 h2 h1-semantic h2-semantic))))

; this function is like 'check-hash', but in addition it prints differences between hashes:
(define-check (check-hash-difference-1 h1 h2 h1-semantic h2-semantic)
  ; h1 -resulted hash, h2 - sample hash, against which we compare the resulted hash
  ; list-order? - Applied in the case, when hash keys are lists. If #t, then the order for elements in list is important, otherwise elements can follow in any order, they should be just of the same set.
  (if (check-hash h1 h2 #t)
    (void)
    (let* ((h1-keys (hash-keys h1))
          (h2-keys (hash-keys h2))
          (h1-but-not-h2 (minus h1-keys h2-keys #:equal-f deep-equal-set?))
          (h2-but-not-h1 (minus h2-keys h1-keys #:equal-f deep-equal-set?))
          (difference-in-keys? (or (not-empty? h1-but-not-h2) (not-empty? h2-but-not-h1)))
          (common-keys (intersect h1-keys h2-keys))
          (different-values (for/fold
                              ((res (hash)))
                              ((k common-keys))
                              (if (equal? (hash-ref h1 k) (hash-ref h2 k))
                                res
                                (hash-union (hash k (list (hash-ref h1 k) (hash-ref h2 k))) res))))
          (different-values? (not (hash-empty? different-values))))
      (fail-check
        (format "Fail: -Different hashes-~n~a~n~a"
            (if difference-in-keys?
              (format "Different keys~n[~a]: ~a~n~n[~a]: ~a~n~n"
                      h1-semantic
                      (list-pretty-string h1-but-not-h2)
                      h2-semantic
                      (list-pretty-string h2-but-not-h1))
              "")
            (if different-values?
              (format "Different values~n[~a,~a]: ~a"
                      h1-semantic
                      h2-semantic
                      (hash-pretty-string different-values))
              "")))
      (void))))

(define-check (check-same-elements? l1 l2)
  (if (empty? (difference l1 l2))
    (void)
    (fail-check
      (format "Fail: Lists are different on elements: ~a" (difference l1 l2)))))
