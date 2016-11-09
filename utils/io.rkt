#lang racket

(provide (all-defined-out))

(require compatibility/defmacro)

(define debug
  (lambda args (apply string-append
    (map
      (lambda (el)
        (cond
          ((number? el) (number->string el))
          ((list? el) (list->string el)) ; list of chars to string
          (else el)))
      args))))

(define (write-file filename str)
  (display-to-file str filename #:exists 'replace))
