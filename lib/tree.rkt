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
