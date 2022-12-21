#lang racket

(require json)
(require net/url)
(require net/uri-codec)
(require "../main.rkt")

(provide (all-defined-out))

(define uniprot-api-base "https://www.uniprot.org")

(define-catch (upload-list ids #:output-format (output-format "text"))
  (let* ((headers (list (format "Content-Type: form-data")))
        (body (string->bytes/utf-8 (format "file=~a&format=~a&from=~a&to=~a" (first ids) output-format "txt" "rdf")))
        (response (port->string
                    (post-impure-port
                      (string->url (str uniprot-api-base "/uploadlists"))
                      body
                      headers
                      ))))
  ; (--- response)
  #t))

; (upload-list (list "O14713"))
