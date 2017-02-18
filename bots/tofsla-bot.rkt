#lang racket

(require "../lib/all.rkt")

; t.me/tofsla_vifsla_bot
; token: 332712470:AAGAXtx-9AfCk16eUloJnhpQh2LIDuweXNc

(define (reply astr)
  (implode
    (map
      (curryr str "sla")
      (split astr " "))
    " "))
