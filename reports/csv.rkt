#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

(define (write-csv-file headers data filename (divisor ","))
  (let* ( (res-header (implode headers divisor))
          (res-body (implode
                      (map
                        (λ (row) (implode (hash-refs row headers "") divisor))
                        data)
                      "\n"))
          (res (str res-header "\n" res-body)))
    (write-file filename res)))
