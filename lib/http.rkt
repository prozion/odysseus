#lang racket

(require net/url)

(provide (all-defined-out))

(define (get-url url)
  (port->string
    (get-pure-port (string->url url))))

(define (get-url-bytes url)
  (port->bytes
    (get-pure-port (string->url url))))
