#lang racket

(require "../../lib/load/all.rkt")

(provide (all-defined-out))

(define-catch (add-label label item)
  (hash-union (hash ':label label) item))
