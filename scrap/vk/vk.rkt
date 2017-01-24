#lang racket

;(require "../../lib/all.rkt")

(define status-output (make-parameter #f))
(define friends-limit (make-parameter #f))

(provide (all-defined-out))
