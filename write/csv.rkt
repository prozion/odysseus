#lang racket

(require compatibility/defmacro)
(require "../main.rkt")

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

; headers :: (List ColumnName)
; data :: (List (Hash ColumnName Value)) | (List (List Value)) | (Value . Value)
(define-catch (get-csv headers data #:delimeter (delimeter ","))
  (let* ( (res-header (string-join (map ->string headers) delimeter))
          (data (if (hash? data) (hash-values data) data))
          (res-body (string-join
                      (map
                        (λ (row)
                          (cond
                            ((hash? row)
                              (string-join (map ->string (hash-refs row headers "")) delimeter))
                            ((list? row)
                              (string-join (map ->string row) delimeter))
                            ((cons? row)
                              (format "~a,~a" (car row) (cdr row)))
                            (else
                              row)))
                        data)
                      "\n")))
      (string-append res-header "\n" res-body)))

(define-catch (write-csv-file headers data filename #:delimeter (delimeter ","))
    (write-file filename (get-csv headers data #:delimeter delimeter)))

(module+ test

  (require rackunit)

  (check-equal? (hash->csv-line (hash 'a 10 'b 20) '(b a))
                "20,10")

)
