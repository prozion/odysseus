#lang racket

(require "seqs.rkt")

(provide (all-defined-out))

(define (farenheit->celsius Tf)
  (* (- Tf 32.0) 5/9))

(define (celsius->farenheit Tc)
  (+ 32.0 (* 9/5 Tc)))
