#lang racket

(require "../../lib/load/all.rkt")

(provide (all-defined-out))

(define (en->ru key)
  (hash-ref synonims key #f))

(define (ru->en key)
  (hash-ref (hash-revert synonims) key #f))

(define synonims
  (@
    "TSH" "ТТГ"
    "MRI" "МРТ"
    "PTH" (list "Паратгормон" "ПТГ" "Паратирин")
    "pancreas" "поджелудочная железа"
    "liver" "печень"
    "thyroid" "щитовидная железа"
))
