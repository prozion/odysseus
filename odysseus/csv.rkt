#lang racket

(require "main.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

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
                              (format "~a~a~a" (car row) delimeter (cdr row)))
                            (else
                              row)))
                        data)
                      "\n")))
      (string-append res-header "\n" res-body)))

(define-catch (write-csv-file headers data filename #:delimeter (delimeter ","))
    (write-file filename (get-csv headers data #:delimeter delimeter)))
