#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

; (@ 'surname "" 'name "" 'tags "")

(define (identify-person . body)
  #t
  ;; identify person by order of arguments:
  ;; 1 surname
  ;; 2 name
)

(define (contains-tag? mark)
  (λ (person)
    (if (@. person.tags)
      (indexof? (@. person.tags) mark)
      #f)))

; tags:
; c - classmate,
(define classmate? (contains-tag? "c"))
; d - died
(define dead? (contains-tag? "d"))
; r - blood relative
(define relative? (contains-tag? "r"))
; u - don't know each other
(define unknown? (contains-tag? "u"))
; v - only virtual acquaintance
(define virtual? (contains-tag? "v"))
; t - related to transhumanism
(define transhumanism? (contains-tag? "t"))
; m - mafia players
(define mafia? (contains-tag? "m"))

(define (has-phone? rec)
  (if (hash-ref rec 'phone  #f) #t #f))

; acquaintances number
(define (acqs x)
  (length
    (clean
      (or-> virtual? dead? unknown?)
      x)))

(define (with-phones x)
  (length
    (filter has-phone? x)))

(define (cyr->translit cyrstr)
  cyrstr)

(define (make-id person)
  (str
    (cyr->translit (@. person.name))
    "_"
    (cyr->translit (@. person.surname))))
