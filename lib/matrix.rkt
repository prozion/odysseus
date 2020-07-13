#lang racket

(require "base.rkt")
(require "controls.rkt")
(require "seqs.rkt")

(require math/array)
(require math/matrix)

(require compatibility/defmacro)

(provide (all-defined-out))

(define (make-col . vs)
  (list->matrix (length vs) 1 vs))
