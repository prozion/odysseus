#lang racket

(require compatibility/defmacro)
(require "../lib/_all.rkt")

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

(define text->csv-text
  (change-text
    (list
      (cons "#f" "")
      (cons "\\" "")
      ; (cons "\"" "'")
      (cons "\"" "\"\"")
    )))

(define-catch (transform-csv-fields csv-items transform-hash)
  (map
    (λ (item)
      (for/fold
        ((res item))
        (((k v) transform-hash))
        (cond
          ((hash-ref res (->symbol k) #f)
              (hash-union
                (hash (->symbol k) (v (hash-ref* res k) res))
                res))
          (else res))))
    csv-items))

;  data: (list (list ...) ...)
(define-catch (write-csv-file headers data filename #:delimeter (delimeter ","))
  (let* ( (res-header (string-join (map ->string headers) delimeter))
          (res-body (string-join
                      (map
                        (λ (row)
                          (cond
                            ((hash? row)
                              (string-join (map ->string (hash-refs row headers "")) delimeter))
                            ((list? row)
                              (string-join (map ->string row) delimeter))
                            (else
                              row)))
                        data)
                      "\n"))
          (res (string-append res-header "\n" res-body)))
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

(module+ test

  (require rackunit)

  (check-equal? (hash->csv-line (hash 'a 10 'b 20) '(b a))
                "20,10")

)
