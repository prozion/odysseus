#lang racket

(require "sorting.rkt")
(require "../load/all.rkt")

(module+ test

  (require rackunit)

  (check-equal? (bubble-sort '(5 2 4 3)) '(2 3 4 5))
  (check-equal? (insertion-sort-1 '(5 2 4 3)) '(2 3 4 5))
  (check-equal? (insertion-sort-2 '(5 2 4 3)) '(2 3 4 5))
  (check-equal? (merge-sort '(5 2 4 3)) '(2 3 4 5))
)

(define random-seq (map random (dup 100 300)))

(print-benchmark (bubble-sort random-seq) "bubble-sort time:")
(print-benchmark (insertion-sort-1 random-seq) "insertion-sort-1 time:")
(print-benchmark (insertion-sort-2 random-seq) "insertion-sort-2 time:")
(print-benchmark (merge-sort random-seq) "merge-sort time:")
