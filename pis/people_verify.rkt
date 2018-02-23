#lang racket

(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (check-all-duplicates field people)
  (clean
    null?
    (not-uniques
      (for/list ((r people)) (hash-ref r field null)))))
