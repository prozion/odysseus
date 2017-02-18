#lang racket

(require net/url)
;(require net/http-client)

(provide (all-defined-out))

(define (get-url url)
  (port->string
    (get-pure-port (string->url url))))

(define (get-url-bytes url)
  (port->bytes
    (get-pure-port (string->url url))))

; http/1.1 spec: https://tools.ietf.org/html/rfc2616
(define (post-url url headers body)
  url)

;(define (post-url url post-data (user-data #f))
;  (let* ( (post #"") ; home=Cosby&favorite+flavor=flies
;          (header (list "" ""))) ; Content-Type: application/json" Authorization: Basic c2lsaWNvbjpGNjIxRjQ3MC05NzMxLTRBMjUtODBFRi02N0E2RjdDNUY0Qjg=
;    (port->string
;      (post-pure-port (string->url url) post header))))
