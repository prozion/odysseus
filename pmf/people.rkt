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

(define (contains-key? akey)
  (λ (person)
    (indexof? (hash-keys person) akey)))

(define (key-equal? akey val)
  (λ (person)
    (and
      (indexof? (hash-keys person) akey)
      (equal? (hash-ref person akey) val))))

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
(define th? (contains-tag? "t"))
; m - mafia players
(define mafia? (contains-tag? "m"))

(define has-phone? (contains-key? 'phone))
(define has-email? (contains-key? 'email))

(define (phone=? val) (key-equal? 'phone val))
(define (city=? val) (key-equal? 'city val))

; acquaintances number
(define (acqs x)
  (length
    (clean
      (or-> virtual? dead? unknown?)
      x)))

(define (with-phones x)
    (filter has-phone? x))

(define (cyr->translit cyrstr)
  cyrstr)

(define (make-id person)
  (str
    (cyr->translit (@. person.name))
    "_"
    (cyr->translit (@. person.surname))))

;; requests:
; ods-query "mafia?"
; ods-query "(and- mafia? transhumanism?)
; ods-query '(and- mafia? (city= "Мурманск"))

; ods-query '(and-> mafia? (city=? "Мурманск")'

(define (field->str person fieldname (mode 'first) (default #f))
  (let ((fieldval (hash-ref person fieldname default)))
      (cond
        ((list? fieldval)
          (cond
            ((equal? mode 'first) (first fieldval))
            (else (implode fieldval ", ")))) ; 'all and others
        (else fieldval))))


(define (print-person person)
  (displayln (format  "~a ~a"
                      (field->str person 'name 'first "")
                      (or
                        (field->str person 'surname)
                        (field->str person 'nick)
                        "")))
  (displayln (field->str person 'email 'all "--"))
  (displayln (field->str person 'phone 'all "--"))
  (newline))
