#lang racket

(require "../lib/all.rkt")
(require "../graphics/console.rkt")

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
; g - gay/lesbian
(define gay? (contains-tag? "g"))

(define has-phone? (contains-key? 'phone))
(define has-email? (contains-key? 'email))
(define has-city? (contains-key? 'city))
(define has-sn? (contains-key? 'sn))

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
  (let* ( (fieldval (hash-ref person fieldname default))
          (fieldval (if (and fieldval (indexof? fieldval " "))
                      (split fieldval " ")
                      fieldval)))
      (cond
        ((list? fieldval) ; variants in a list
          (cond
            ((equal? mode 'first) (first fieldval))
            (else (implode fieldval ", ")))) ; 'all and others
        (else fieldval))))

(define (person->string person query fields)
  (let ((name (field->str person 'name 'first ""))
        (surname (field->str person 'surname 'first ""))
        (nick (field->str person 'nick 'first "n/a")))
  (string-append
    (format "~a" (string-text-color 'yellow))
            (if (and (nil? name) (nil? surname))
              (format " ~a~n" nick)
              (if (nil? surname)
                (if (not (nil? nick))
                  (format " ~a ~a~n" name nick)
                  (format " ~a~n" name))
                (format " ~a ~a~n" name surname)))
    (format "~a" (string-text-color 'grey))
    (apply string-append
      (map
        (λ (x) (format " ~a~n" (field->str person (string->symbol x) 'all "-")))
        fields))
    (format "~a" (string-text-color 'default)))))
