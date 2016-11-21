#lang racket

(require "alist.rkt")

(provide (all-defined-out))

(define (type? x)
  (cond
    ((number? x) 'number)
    ((string? x) 'string)
    ((alist? x) 'alist)
    ((list? x) 'list)
    ((pair? x) 'pair)
    ((char? x) 'char)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    ((syntax? x) 'syntax)
    ((vector? x) 'vector)
    ((hash? x) 'hash) ; STX hash?
    ((path? x) 'path) ; STX path?
    (else #f)))
