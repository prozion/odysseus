#lang racket

(require net/url)

(provide (all-defined-out))

(define (get-url url)
  (port->bytes
    (get-pure-port (string->url url))))
