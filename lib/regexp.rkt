#lang racket

(require "base.rkt")
(require "seqs.rkt")

(provide (all-defined-out))

(define (get-matches re astr)
  (let ((re
          (if (or (regexp? re) (pregexp? re))
                re
                (pregexp re))))
    (for/fold
      ((res (list)))
      ((match-position (regexp-match-positions* re astr)))
      (pushr
        res
        (regexp-match
          re
          astr
          (car match-position))))))
