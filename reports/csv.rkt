#lang racket

(require compatibility/defmacro)
(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (hash->csv-line h headers (divisor ","))
  (for/fold
    ((res (format "~a" (hash-ref h (car headers) ""))))
    ((header (cdr headers)))
    (format "~a~a~a"
              res
              divisor
              ; remove "", if no value:
              (let ((val (hash-ref h header #f)))
                (if val
                  (format "\"~a\"" (string-replace (->string val) "\"" ""))
                  "")))))

(define (write-csv-file headers data filename (divisor ","))
  (let* ( (res-header (implode headers divisor))
          (res-body (implode
                      (map
                        (λ (row)
                          (implode
                            (cond
                              ((hash? row)
                                (hash-refs row headers ""))
                              ((list? row)
                                row)
                              (else
                                row))
                            divisor))
                        data)
                      "\n"))
          (res (str res-header "\n" res-body)))
    (write-file filename res)))
