#lang racket

(require rackunit)
(require "../list.rkt")
(require "../hash.rkt")
(require "../checks.rkt")
(require "../type.rkt")
(require "../regexp.rkt")
(require "../tree.rkt")
(require "../debug.rkt")

(define h (hash 'a (hash 'aa 10 'ab 20) 'b (hash 'ba (hash 'baa 300 'bab 30))))

(check-true (check-hash (hash) (hash)))
(check-true (check-hash (@ 'a 1 'b 2) (hash 'a 1 'b 2)))
(check-true (check-hash (hash 'b 2 'a (- 4 3)) (hash 'a 1 'b 2)))
(check-false (check-hash (hash 'a 10 'b 2) (hash 'a 1 'b 2)))
(check-false (check-hash (hash 'a 1 'b 2 'c 3) (hash 'a 1 'b 2)))
; different element orders in the lists:
(check-false (check-hash (hash 'a '(1 2 3) 'b 2 'c '(1 3 5)) (hash 'a '(2 1 3) 'b 2 'c '(1 3 5))))
(check-true (check-hash (hash 'a '(1 2 3) 'b 2 'c '(1 3 5)) (hash 'a '(2 1 3) 'b 2 'c '(1 3 5)) #:list-any-order? #t))
(check-true (check-hash (hash 'a '(1 (2 5) 3) 'b 2 'c '(1 3 5)) (hash 'a '((5 2) 1 3) 'b 2 'c '(1 3 5)) #:list-any-order? #t))

(check-hash-equal? (@ 'a 1 'b 2) (hash 'a 1 'b 2))
(check-hash-equal? (@ "" null #f empty 'a 1 'b 2) (hash 'a 1 'b 2))
(check-hash-equal? (@ (when #f 'a) 'b 10 'c 20) (hash 'b 10 'c 20))
(check-hash-equal? (@ 'a 1 'b 2) (hash 'a 1 'b 2))
(check-hash-equal? (@ "" null #f empty 'a 1 'b 2) (hash 'a 1 'b 2))

(check-equal? ($ a (hash 'a 10 'b 20)) 10)
(check-equal? ($ a 50) #f)
(check-equal? ($ a #f) #f)
(check-equal? ($ a.ab (hash 'a (hash 'aa 200 'ab 300) 'b 20)) 300)
(check-equal? ($ a.ab.cd.ef (hash 'a (hash 'aa 200 'ab 300) 'b 20)) #f)
(check-false ($ c (hash 'a 10 'b 20)))
(check-false ($ c 100))
(check-equal? ($ a.aa
                (hash 'a (list (hash 'aa 10 'bb 25) (hash 'aa 17 'cc 12))))
              (list 10 17))
(check-equal? ($ a.aa.aac
                (hash 'a (list (hash 'aa (hash 'aac 42) 'bb 25) (hash 'aa 17 'cc 12))))
              42)

(check-true (hash-empty? (hash)))
(check-false (hash-empty? (hash 'a 10)))

(check-equal? (hash-ref* (hash 'a 10 'b 20) 'b) 20)
(check-equal? (hash-ref* (hash 'a 10 'b 20) "b") 20)
(check-equal? (hash-ref* (hash 'a 10 "b" 20) 'b) 20)
(check-equal? (hash-ref* (hash 'a 10 "b" 20) 'c 100) 100)

(check-hash-equal?
  (hash-set* (hash) '(a) 1)
  (hash 'a 1))
(check-hash-equal?
  (hash-set* (hash) '(a b c) 1)
  (hash 'a (hash 'b (hash 'c 1))))
(check-hash-equal?
  (hash-set* (hash 'b 2) '(a) 1)
  (hash 'b 2 'a 1))
(check-hash-equal?
  (hash-set* (hash 'b 2) '(a aa aab) 1)
  (hash 'b 2 'a (hash 'aa (hash 'aab 1))))
(check-hash-equal?
  (hash-set* (hash 'b 2) '(b) 1)
  (hash 'b 1))
(check-hash-equal?
  (hash-set* (hash 'b 2) '() 1)
  (hash 'b 2))
(check-hash-equal?
  (hash-set* (hash 'b 2 'c (hash 'ca 7)) '(c ca) 3)
  (hash 'b 2 'c (hash 'ca 3)))
(check-hash-equal?
  (hash-set* (hash 'b 2 'c (hash 'ca 7)) '(c cb) 9)
  (hash 'b 2 'c (hash 'ca 7 'cb 9)))
(check-hash-equal?
  (hash-set* (hash 'b 2 'c (hash 'ca '(1 2 3) 'cb 9)) '(c ca) 9)
  (hash 'b 2 'c (hash 'ca 9 'cb 9)))
(check-hash-equal?
  (hash-set* (hash 'b 2 'c (hash 'ca '(1 2 3) 'cb 9)) '(c ca) '(9 8 7))
  (hash 'b 2 'c (hash 'ca '(9 8 7) 'cb 9)))

(define nested-hash (hash
                      'a 10
                      'b (hash
                            'ba 3
                            'bb (hash
                                  "quux" '(17 34)
                                  10 5)
                            "bc" (hash (hash) (hash))
                            'bd (hash 1 5))
                      'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

; one-element chain
(check-hash-equal?
  (hash-update* nested-hash (list 'a) (const 30))
  (hash
    'a 30
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34)
                10 5)
          "bc" (hash (hash) (hash))
          'bd (hash 1 5))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

; a deep chain with list value
(check-hash-equal?
  (hash-update* nested-hash (list 'b 'bb "quux") (curryr append-elements 42))
  (hash
    'a 10
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34 42)
                10 5)
          "bc" (hash (hash) (hash))
          'bd (hash 1 5))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

; a deep chain with a hash key
(check-hash-equal?
  (hash-update* nested-hash (list 'c (hash "foo" "bar")) (curryr hash-set 'ccd 3))
  (hash
    'a 10
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34)
                10 5)
          "bc" (hash (hash) (hash))
          'bd (hash 1 5))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2 'ccd 3))))

; a chain with an number key
(check-hash-equal?
  (hash-update* nested-hash (list 'b 'bd 1) (const 100))
  (hash
    'a 10
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34)
                10 5)
          "bc" (hash (hash) (hash))
          'bd (hash 1 100))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

; a chain with an empty hash key
(check-hash-equal?
  (hash-update* nested-hash (list 'b "bc" (hash)) (const 100))
  (hash
    'a 10
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34)
                10 5)
          "bc" (hash (hash) 100)
          'bd (hash 1 5))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

; incorrect path
(check-hash-equal?
  (hash-update* nested-hash (list 'b 'bc 'd) (const 100))
  (hash
    'a 10
    'b (hash
          'ba 3
          'bb (hash
                "quux" '(17 34)
                10 5)
          "bc" (hash (hash) (hash))
          'bd (hash 1 5))
    'c (hash (hash "foo" "bar") (hash 'cca 1 'ccb 2))))

(check-equal? (hash-path (hash 'a 1 'b 10 'c 100) 'c) 100)
(check-equal? (hash-path h 'b 'ba 'bab) 30)
(check-false (hash-path h 'c 'ba 'baa))
(check-false (hash-path h 'a 'ac))
(check-false (hash-path h 'a 'ab 'aba))

(check-equal? (hash-refs (hash 'b 20 'a 10 'c 40 'd 30) (list 'a 'b 'd)) (list 10 20 30))
(check-equal? (hash-refs (hash 'b 20 'a 10 'c 40 'd 30) (list 'a 'b 'd 'e)) (list 10 20 30 #f))
(check-equal? (hash-refs (hash) (list 'a 'b 'd 'e)) (list #f #f #f #f))
(check-equal? (hash-refs (hash 'b 20 'a 10 'c 40 'd 30) (list 'a 'b 'd 'e) "default") (list 10 20 30 "default"))
(check-equal? (hash-refs (hash) (list 'a 'b 'd 'e) "") (list "" "" "" ""))

(check-hash-equal?
  (list->hash '((1 1 2 3 4) (2 5 6 7 8)) (list 'key 'a 'b 'c 'd) #:columns-exclude '(b d))
  (hash 1 (hash 'a 1 'c 3) 2 (hash 'a 5 'c 7)))

(check-equal? (@. h.a.aa) 10)
(check-equal? (@. h.c) #f)

(check-hash-equal?
  (@. h)
  h)

; hash-map
(check-hash-equal?
  (hash-map
    (λ (k v) (values k (* v 2)))
    (hash 'a 80 'b 70))
  (hash 'a 160 'b 140))

; hash-map-deep
(check-hash-equal?
  (hash-map-deep
    (λ (k v) (values k (* v 2)))
    (hash 'a (hash 'aa 10 'aaa 100) 'b 70))
  (hash 'a (hash 'aa 20 'aaa 200) 'b 140))

; hash-delete
(check-hash-equal?
  (hash-delete (hash 'a 10 'b 20 'c 40) 'c)
  (hash 'a 10 'b 20))

(check-hash-equal?
  (hash-delete (hash 'a 10 'b 20 'c 40) 'd)
  (hash 'a 10 'b 20 'c 40))

(check-hash-equal?
  (hash-delete (hash (hash 'aa 10 'ab 20) 10 'b 20 'c 40) (hash 'aa 10 'ab 20))
  (hash 'b 20 'c 40))

; hash-revert
(check-hash-equal?
  (hash-revert (hash 'a 'aa 'b 'bb 'aba 30))
  (hash 'aa 'a 'bb 'b 30 'aba))

(check-hash-equal?
  (hash-revert
    (hash 'a 10))
  (hash
      10 'a))

(check-hash-equal?
  (hash-filter
    (λ (k v) (re-matches? "^\\d\\d?\\.\\d\\d\\.\\d\\d\\d\\d$" k))
    (hash
      "02.1986" "a1"
      "27.04.1986" "a2"
      "01.05.1986" "a3"
      "05.1986" "a4"))
  (hash
      "27.04.1986" "a2"
      "01.05.1986" "a3"))

(check-hash-equal?
  (hash-clean
    (λ (k v) (re-matches? "^\\d\\d?\\.\\d\\d\\.\\d\\d\\d\\d$" k))
    (hash
      "02.1986" "a1"
      "27.04.1986" "a2"
      "01.05.1986" "a3"
      "05.1986" "a4"))
    (hash
      "02.1986" "a1"
      "05.1986" "a4"))

(check-hash-equal?
  (hash-remove-keys (hash 'a 10 'b 20 'c 30 'd 40 'e 50) '(a c d))
  (hash 'b 20 'e 50))

(check-hash-equal?
              (hash-keys-substitute (hash 'a 10 'b 20) '(a b) '(c d))
              (hash 'c 10 'd 20))
