#lang racket

(provide (all-defined-out))

(define-syntax (for/fold/idx stx)
  (syntax-case stx ()
    [(_ (s start-value) (i sequence) body)
      (datum->syntax stx
        `(for/fold
          ([,#'s ,#'start-value])
          ([,#'i ,#'sequence] [$idx (range (length ,#'sequence))])
          ,#'body))
    ]
    ;[(_ (s start-value) ((k v) sequence) body)
    ;  (datum->syntax stx
    ;    `(for/fold
    ;      ([,#'s ,#'start-value])
    ;      ([(,#'k ,#'v) ,#'sequence] [$idx (range (length (hash-keys ,#'sequence)))])
    ;      ,#'body))
    ;]
))
