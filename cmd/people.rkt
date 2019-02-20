#lang racket

(require "../lib/_all.rkt")
(require (for-syntax "../lib/seqs.rkt"))
(require compatibility/defmacro)
(require "../graphics/console.rkt")
(require "../report/csv.rkt")

(provide (all-defined-out))

(define (has-all-fields fields)
  (λ (p)
    (andmap
      (λ (f) (indexof? (hash-keys p) (string->symbol f)))
      fields)))

(define (ppl-output fields people-sublist (csvfile ""))
  (if (nil? csvfile)
    (display (people->string people-sublist fields))
    (write-csv-file
      (merge
        (list 'surname 'name)
        (map string->symbol fields))
      people-sublist
      csvfile)))

(define (check-all-duplicates field people)
  (clean
    null?
    (not-uniques
      (for/list ((r people)) (hash-ref r field null)))))

; (@  'surname "" 'name "")

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

(define (key-equal? akey val (regexp? #f))
  (λ (person)
    (let* ( (akey-val (hash-ref person akey ""))
            (akey-vals (if (list? akey-val) akey-val (split akey-val " "))))
      (and
        (indexof? (hash-keys person) akey)
        (or
          (equal? akey-val val)
          (indexof? akey-vals val)
          (if regexp?
            (regexp-indexof? akey-vals val)
            #f))))))

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
; t - probably related to transhumanism, T - strongly related to transhumanism
(define th? (contains-tag? "T"))
(define transhumanist? th?)
(define th?? (contains-tag? "t"))
; m - mafia players
(define mafia? (contains-tag? "m"))
; g - gay/lesbian
(define gay? (contains-tag? "g"))
; p - probable candidate to link each other in PA-network
(define pan? (contains-tag? "p"))
; hubber - person that links many people and could (would) have introduced them, network builder
(define hubber? (contains-tag? "h"))

(define lack-of-info? (contains-tag? "?"))
(define medical-related? (contains-tag? "M"))

(define has-phone? (contains-key? 'phone))
(define has-email? (contains-key? 'email))
(define has-city? (contains-key? 'city))
(define has-sn? (contains-key? 'sn))

(define (phone=? val) (key-equal? 'phone val))
(define (city=? val) (key-equal? 'city val))
(define (name=? val) (key-equal? 'name val))
(define (surname=? val) (key-equal? 'surname val))

(define (by-name-surname name-surname)
  (let* (
        (vals (split name-surname " "))
        (part1 (nth vals 1))
        (part2 (nth vals 2)))
      (cond
        ((nil? part2)
          (or-> (key-equal? 'name part1 #t) (key-equal? 'surname part1 #t)))
        (else
          (or->
            (and->
              (key-equal? 'name part1 #t)
              (key-equal? 'surname part2 #t))
            (and->
              (key-equal? 'name part2 #t)
              (key-equal? 'surname part1 #t)))))))

(define (person-signature p)
  (format "~a ~a ~a"
    (hash-ref p 'name "")
    (hash-ref p 'surname "")
    (hash-ref p 'nick "")))

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

(define (get-by-contact-level people level)
  (filter
    (λ (person) (and ($ l person) (= (->number ($ l person)) level)))
    people))

(define-catch (get-name person)
  (let* ((name_surname (split (->string ($ id person)) "_")))
    (or ($ name person) (and (not-empty? name_surname) (first name_surname)))))

(define-catch (get-surname person)
  (let* ((name_surname (split (->string ($ id person)) "_")))
    (or ($ surname person) (and (> (length name_surname) 1) (second name_surname)))))

(define (get-surname-name-str person)
  (format "~a ~a" (get-surname person) (get-name person)))

(define (get-name-surname-str person)
  (let ((name (or (get-name person) ""))
        (surname (or (get-surname person) ""))
        (str_possible? (or (get-name person) (get-surname person))))
    (if str_possible?
      (format "~a ~a" name surname)
      #f)))

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

(define (esc-sec val)
  (if (console-output)
    (format "~a" val)
    ""))

(define (get-informative-fields person)
  (let ((fields-to-check (split "sn email skype phone" " ")))
    (filter (λ (field) (hash-ref person (->symbol field) #f)) fields-to-check)))

(define (person->string person fields)
  (format "~a~a~n~a~a~n~a"
    (esc-sec (string-text-color 'yellow))
    (if (scalar? (car fields))
      (hash-ref* person (car fields))
      ((car fields) person))
    (esc-sec (string-text-color 'grey))
    (for/fold
      ((res ""))
      ((field (cdr fields)))
      (let ((value (if (scalar? field)
                      (hash-ref* person field)
                      (field person))))
        (if value (str res value "\n") res)))
    (esc-sec (string-text-color 'default))))

(define (people->string subpeople fields)
  (for/fold
    ((res ""))
    ((person subpeople))
    (str res (person->string person fields))))
