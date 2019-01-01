#lang racket

(require compatibility/defmacro)
(require "../lib/load/all.rkt")

(provide (all-defined-out))

(define (hash->csv-line h headers (delimeter ","))
  (for/fold
    ((res (format "~a" (hash-ref h (car headers) ""))))
    ((header (cdr headers)))
    (format "~a~a~a"
              res
              delimeter
              ; remove "", if no value:
              (let ((val (hash-ref h header #f)))
                (if val
                  (format "\"~a\"" (string-replace (->string val) "\"" ""))
                  "")))))

(define-catch (write-csv-file headers data filename #:delimeter (delimeter ","))
  (let* ( (res-header (implode headers delimeter))
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
                            delimeter))
                        data)
                      "\n"))
          (res (str res-header "\n" res-body)))
    (write-file filename res)))

(define (write-csv-file* #:columns columns #:data data #:csvfile filename #:delimeter (delimeter ","))
  (let* ( (headers (map car columns))
          (res-header (implode headers delimeter))
          (res-body (implode
                      (map
                        (λ (row)
                          (implode
                            (cond
                              ((hash? row)
                                (hash-refs row (map cdr columns) ""))
                              ((list? row)
                                row)
                              (else
                                row))
                            delimeter))
                        data)
                      "\n"))
          (res (str res-header "\n" res-body)))
    (write-file filename res)))
