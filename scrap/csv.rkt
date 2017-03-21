#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

(define (csv-file->list filename #:delimeter (delimeter ",") #:headers (headers #t))
  (let
      ((first-break-list
        (map
          (λ (x) (split x delimeter))
          (split (read-file filename) "\n"))))
    (if headers
      (merge  (list (first first-break-list))
              (transpose (rest first-break-list)))
      (push  empty
              (transpose first-break-list)))))
