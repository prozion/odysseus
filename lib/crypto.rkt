#lang racket

(require sha)

(provide (all-defined-out))

(define (get-sha astr)
  (bytes->hex-string (sha1 (string->bytes/utf-8 astr))))
