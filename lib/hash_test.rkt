#lang racket

(module+ test

  (require rackunit)
  (require "seqs.rkt")
  (require "hash.rkt")
  (require "checks.rkt")
  (require "alist.rkt")
  (require "type.rkt")
  (require "regexp.rkt")
  (require "tree.rkt")

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

  (check-hash-equal? (hash-sym a b c d) (hash "a" "b" "c" "d"))

  (check-true (hash-key? (hash 'a 10 'b 20) 'a))
  (check-false (hash-key? (hash 'a 10 'b 20) 'c))

  (check-true (hash-empty? (hash)))
  (check-false (hash-empty? (hash 'a 10)))

  (check-equal? (hash-ref* (hash 'a 10 'b 20) 'b) 20)
  (check-equal? (hash-ref* (hash 'a 10 'b 20) "b") 20)
  (check-equal? (hash-ref* (hash 'a 10 "b" 20) 'b) 20)
  (check-equal? (hash-ref* (hash 'a 10 "b" 20) 'c 100) 100)

  (check-hash-equal? (@clean 'a 1 'b 2 'c "") (hash 'a 1 'b 2))

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

  (check-equal? (hash-pair (hash 'a 10 'b 20) 'a) (cons 'a 10))
  (check-equal? (hash-pair (hash (hash 'u 3 'w 30) 100 'b 20) (hash 'u 3 'w 30)) (cons (hash 'u 3 'w 30) 100))

  (check-true (alist? (hash->alist (hash 'a 1 'b 2))))
  (check-equal? (hash->alist (hash 'a 1 'b 2)) '((a 1) (b 2)))

  ; (check-equal? (hash->cons (hash 'a 1 'b 2)) (list (a . 1) (b . 2))) Racket function hash->list does the same

  (check-hash-equal?
    (list->hash '((1 1 2 3 4) (2 5 6 7 8)) (list 'key 'a 'b 'c 'd) #:columns-exclude '(b d))
    (hash 1 (hash 'a 1 'c 3) 2 (hash 'a 5 'c 7)))

  (check-equal? (hash->string (hash 'a 1 'b 2)) "a=1, b=2")
  (check-equal? (hash->string (hash 'a 1 'b 2 'c 10) #:delimeter " AND ") "a=1 AND c=10 AND b=2")
  (check-equal? (hash->string (hash 'a 1 'b 2) #:delimeter " AND " #:prefix "n.") "n.a=1 AND n.b=2")
  (check-equal? (hash->string (hash 'a 1 'b 2) #:delimeter ", " #:prefix "n." #:equal-sign ": ") "n.a: 1, n.b: 2")
  (check-equal? (hash->string (hash 'a 1 'b "Polyphem") #:delimeter ", " #:prefix "n." #:equal-sign ": ") "n.a: 1, n.b: \"Polyphem\"")

  (check-equal? (hash-print-json (hash 'a 1 'b 2)) "{a: 1, b: 2}")

  (check-equal? (@. h.a.aa) 10)
  (check-equal? (@. h.c) #f)

  (check-hash-equal?
    (@. h)
    h)

  (check-hash-equal?
    (@clean 'a 1 'b 2 'c "")
    (hash 'a 1 'b 2))

; hash-map
  (check-hash-equal?
    (hash-map
      (λ (k v) (values k (* v 2)))
      (hash 'a 80 'b 70))
    (hash 'a 160 'b 140))

; deep-hash-map
  (check-hash-equal?
    (deep-hash-map
      (λ (k v) (values k (* v 2)))
      (hash 'a (hash 'aa 10 'aaa 100) 'b 70))
    (hash 'a (hash 'aa 20 'aaa 200) 'b 140))

; hash-substitute
  (check-hash-equal?
    (hash-substitute (hash 'a 80 'b 70) (cons 'b 130))
    (hash 'a 80 'b 130))

  (check-hash-equal?
    (hash-substitute (hash 'a 80 'b '(70 30)) (cons 'b '(130 180)))
    (hash 'a 80 'b '(130 180)))

  (check-hash-equal?
    (hash-substitute (hash 'a 80 'b 30) (cons 'c 40))
    (hash 'a 80 'b 30 'c 40))

  (check-hash-equal?
    (hash-substitute (hash 'a 80 'b 70) (list (cons 'b 150) (cons 'a 10) (cons 'c 2)))
    (hash 'a 10 'b 150 'c 2))

  (check-hash-equal?
    (hash-substitute (hash 'a 80 'b 70) (list (cons 'b 130) (cons 'a '(1 2)) (cons 'c 2)))
    (hash 'a '(1 2) 'b 130 'c 2))

; hash-insert
  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) null)
    (hash 'a 10 'b 20))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) null #:overwrite #f)
    (hash 'a 10 'b 20))

  (check-hash-equal?
    (hash-insert null (cons 'c 30))
    (hash 'c 30))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (cons 'c 30))
    (hash 'a 10 'b 20 'c 30))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (cons 'c 30) #:overwrite #f)
    (hash 'a 10 'b 20 'c 30))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (cons 'b 100))
    (hash 'a 10 'b 20))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (cons 'c '(30 20)))
    (hash 'a 10 'b 20 'c '(30 20)))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (list 'c 50 60))
    (hash 'a 10 'b 20 'c '(50 60)))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (list (cons 'a 80) (cons 'c 50) (cons 'd 40)))
    (hash 'a 10 'b 20 'c 50 'd 40))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (list (cons 'a 80) (cons 'c 50) (cons 'd 40)) #:overwrite #f)
    (hash 'a 10 'b 20 'c 50 'd 40))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (list (cons 'a 80) (cons 'c 50) (cons 'b 40)) #:overwrite #t)
    (hash 'a 80 'b 40 'c 50))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20) (list (cons 'a 80) (cons 'c '(50 60)) (cons 'd 40)))
    (hash 'a 10 'b 20 'c '(50 60) 'd 40))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20 'c '(5 10)) (cons 'c '(30 20)))
    (hash 'a 10 'b 20 'c '(5 10)))

  (check-hash-equal?
    (hash-insert (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10)) (cons 'c (hash 'cc 30 'cd 20)))
    (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10)))

; hash-insert-fuse
  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20) null)
    (hash 'a 10 'b 20))

  (check-hash-equal?
    (hash-insert-fuse null (cons 'c 30))
    (hash 'c 30))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20) (cons 'c 30))
    (hash 'a 10 'b 20 'c 30))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20) (cons 'c '(30 20)))
    (hash 'a 10 'b 20 'c '(30 20)))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20 'c '(5 10)) (cons 'c '(30 20)))
    (hash 'a 10 'b 20 'c '(5 10 30 20)))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20 'c "foo") (cons 'c "bar"))
    (hash 'a 10 'b 20 'c "foobar"))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10)) (cons 'c (hash 'cc 30 'cd 20)))
    (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10 'cc 30 'cd 20)))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10)) (cons 'c (hash 'ca 30 'cd 20)))
    (hash 'a 10 'b 20 'c (hash 'ca 5 'cb 10 'cd 20)))

  (check-hash-equal?
    (hash-insert-fuse (hash 'a 10 'b 20 'c 40) (cons 'c 70))
    (hash 'a 10 'b 20 'c 40))

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

; hash-delete-f
  (check-hash-equal?
    (hash-delete-f (hash 'a 10 'b 20 'c 40) (λ (k) (equal? k 'c)))
    (hash 'a 10 'b 20))

  (check-hash-equal?
    (hash-delete-f (hash 'a 10 'b 20 'c 40) (λ (k) (equal? k 'd)))
    (hash 'a 10 'b 20 'c 40))

  (check-hash-equal?
    (hash-delete-f (hash 'aa 1 'ab 2 'b 20 'c 40) (λ (k) (indexof? '(aa ab) k)))
    (hash 'b 20 'c 40))

; hash-delete-all
  (check-hash-equal?
    (hash-delete-all (hash 'a 10 'b 20 'c 40 'd 80) '(c b))
    (hash 'a 10 'd 80))

; hash-revert
  (check-hash-equal?
    (hash-revert (hash 'a 'aa 'b 'bb 'aba 30))
    (hash 'aa 'a 'bb 'b 30 'aba))

  (check-hash-equal?
    (hash-revert
      (hash 'a 10))
    (hash
        10 'a))

; sequence-fuse
  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20) (hash 'c 30))
      (hash 'a 10 'b 20 'c 30))

  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20 'c 30) (hash 'a 40 'd 50))
      (hash 'a 10 'b 20 'c 30 'd 50))

  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20 'c 30) (hash 'a 40 'd 50) #:overwrite #f)
      (hash 'a 10 'b 20 'c 30))

  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20 'c 30) (hash 'a 40 'd 50) #:overwrite 'fuse)
      (hash 'a 10 'b 20 'c 30 'd 50))

  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20 'c 30) (hash 'a 40 'd 50) #:overwrite 'fuse-overwrite)
      (hash 'a 40 'b 20 'c 30 'd 50))

  (check-hash-equal?
      (sequence-fuse (hash 'a 10 'b 20 'c 30) (hash 'a 40 'd 50) #:overwrite #f)
      (hash 'a 10 'b 20 'c 30))

  (check-equal? (sequence-fuse (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
  (check-equal? (sequence-fuse (list 1 2 3) (list 1 4 5 6 3)) (list 1 2 3 1 4 5 6 3))
  (check-equal? (sequence-fuse (list 1 2 3) (list 1 4 5 6 3) #:overwrite #f) (list 1 2 3))
  (check-equal? (sequence-fuse (list 1 2 3) (list 1 4 5 6 3) #:overwrite 'fuse) (list 1 2 3 1 4 5 6 3))
  (check-equal? (sequence-fuse (list 1 2 3) (list 1 4 5 6 3) #:overwrite 'fuse-overwrite) (list 1 2 3 1 4 5 6 3))
  (check-equal? (sequence-fuse (list 1 2 3) (list 1 4 5 6 3) #:overwrite #t) (list 1 4 5 6 3))

  (check-equal? (sequence-fuse (list 1 2 3) 3 #:overwrite #f) (list 1 2 3))
  (check-equal? (sequence-fuse (list 1 2 3) 3 #:overwrite 'fuse) (list 1 2 3 3))
  (check-equal? (sequence-fuse (list 1 2 3) 3 #:overwrite 'fuse-overwrite) (list 1 2 3 3))
  (check-equal? (sequence-fuse (list 1 2 3) 3 #:overwrite #t) 3)

  (check-equal? (sequence-fuse 'a (list 1 2 3) #:overwrite #f) 'a)
  (check-equal? (sequence-fuse 'a (list 1 2 3) #:overwrite 'fuse) (list 'a 1 2 3))
  (check-equal? (sequence-fuse 'a (list 1 2 3) #:overwrite 'fuse-overwrite) (list 'a 1 2 3))
  (check-equal? (sequence-fuse 'a (list 1 2 3) #:overwrite #t) (list 1 2 3))

  (check-equal? (sequence-fuse 'a (hash 'a 10 'b 20) #:overwrite #f) 'a)
  (check-equal? (sequence-fuse 'a (hash 'a 10 'b 20) #:overwrite 'fuse) 'a)
  (check-hash-equal? (sequence-fuse 'a (hash 'a 10 'b 20) #:overwrite 'fuse-overwrite) (hash 'a 10 'b 20))
  (check-hash-equal? (sequence-fuse 'a (hash 'a 10 'b 20) #:overwrite #t) (hash 'a 10 'b 20))

  (check-hash-equal? (sequence-fuse (hash 'a 10 'b 20) 'a #:overwrite #f) (hash 'a 10 'b 20))
  (check-hash-equal? (sequence-fuse (hash 'a 10 'b 20) 'a #:overwrite 'fuse) (hash 'a 10 'b 20))
  (check-equal? (sequence-fuse (hash 'a 10 'b 20) 'a #:overwrite 'fuse-overwrite) 'a)
  (check-equal? (sequence-fuse (hash 'a 10 'b 20) 'a #:overwrite #t) 'a)

; hash-union
  (check-hash-equal?
    (hash-union (hash 'a 10 'b 20) (hash 'c 30 'a 100 'd 2))
    (hash 'a 10 'b 20 'c 30 'd 2))

  (check-hash-equal?
    (hash-union #:overwrite #f (hash 'a 10 'b 20) (hash 'c 30 'a 100 'd 2))
    (hash 'a 10 'b 20 'c 30 'd 2))

  (check-hash-equal?
    (hash-union (hash 'a 10 'b 20) (hash) (hash 'c 30 'a 100 'd 2) (hash 'f 7 'p 18 'c 40 'b 80))
    (hash 'a 10 'b 20 'c 30 'd 2 'f 7 'p 18))

  (check-hash-equal?
    (hash-union #:overwrite #f (hash 'a 10 'b 20) (hash) (hash 'c 30 'a 100 'd 2) (hash 'f 7 'p 18 'c 40 'b 80))
    (hash 'a 10 'b 20 'c 30 'd 2 'f 7 'p 18))

  (check-hash-equal?
    (hash-union #:overwrite #t (hash 'a 10 'b 20 'c 5 'g 17) (hash) (hash 'c 30 'a 100 'd 2) (hash 'f 7 'p 18 'c 40 'b 80))
    (hash 'a 100 'b 80 'c 40 'd 2 'f 7 'p 18 'g 17))

  (check-hash-equal?
    (hash-union (hash 'a 10 'b 20) (hash 'c 30 'a 100 'd 2) (hash 'f 7 'p 18 'c 40 'b 80))
    (hash 'a 10 'b 20 'c 30 'd 2 'f 7 'p 18))

  (let ((e1 (hash-union (hash 'a (hash 'aa 10 'ab 20) 'b 20) (hash 'c 30 'a (hash 'aa 300 'ac 400) 'd 2)))
        (e2 (hash 'a (hash 'aa 10 'ab 20) 'b 20 'c 30 'd 2)))
      (check-hash-equal? e1 e2))

  (let ((e1 (hash-union #:overwrite #t (hash 'a (hash 'aa 10 'ab 20) 'b 20) (hash 'c 30 'a (hash 'aa 300 'ac 400) 'd 2)))
        (e2 (hash 'a (hash 'aa 300 'ac 400) 'b 20 'c 30 'd 2)))
      (check-hash-equal? e1 e2))

  (let ((e1 (hash-union #:overwrite 'fuse (hash 'a (hash 'aa 10 'ab 20) 'b 20) (hash 'c 30 'a (hash 'aa 300 'ac 400) 'd 2)))
        (e2 (hash 'a (hash 'aa 10 'ab 20 'ac 400) 'b 20 'c 30 'd 2)))
      (check-hash-equal? e1 e2))

  (let ((e1 (hash-union #:overwrite 'fuse-overwrite (hash 'a (hash 'aa 10 'ab 20) 'b 20) (hash 'c 30 'a (hash 'aa 300 'ac 400) 'd 2)))
        (e2 (hash 'a (hash 'aa 300 'ab 20 'ac 400) 'b 20 'c 30 'd 2)))
      (check-hash-equal? e1 e2))

  (let ((e3 (hash-union
              #:overwrite #f
              (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12))
                'b 20
                'e '(1 2 3))
              (hash
                'a (hash
                      'aa 300
                      'ab (hash
                            'aba 1000
                            'abc -16)
                      'ac 400)
                'b (hash
                      'ba 33) ; hash will not supersede a number
                'c 30
                'd 2
                'e '(3 4 5))))
        (e4 (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12))
                'b 20
                'c 30
                'd 2
                'e '(1 2 3))))
      (check-hash-equal? e3 e4))

  (let ((e3 (hash-union
              #:overwrite 'fuse
              (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12))
                'b 20
                'e '(1 2 3))
              (hash
                'a (hash
                      'aa 300
                      'ab (hash
                            'aba 1000
                            'abc -16)
                      'ac 400)
                'b (hash
                      'ba 33) ; hash will not supersede a number
                'c 30
                'd 2
                'e '(3 4 5))))
        (e4 (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12
                            'abc -16)
                      'ac 400)
                'b 20
                'c 30
                'd 2
                'e '(1 2 3 3 4 5))))
      (check-hash-equal? e3 e4))

  (let ((e3 (hash-union
              #:overwrite 'fuse-overwrite
              (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12))
                'b 20
                'e '(1 2 3))
              (hash
                'a (hash
                      'aa 300
                      'ab (hash
                            'aba 1000
                            'abc -16)
                      'ac 400)
                'b (hash
                      'ba 33) ; hash will not supersede a number
                'c 30
                'd 2
                'e '(3 4 5))))
        (e4 (hash
                'a (hash
                      'aa 300
                      'ab (hash
                            'aba 1000
                            'abb -12
                            'abc -16)
                      'ac 400)
                'b (hash
                      'ba 33)
                'c 30
                'd 2
                'e '(1 2 3 3 4 5))))
      (check-hash-equal? e3 e4))

  (check-hash-equal?
    (hash-union null (hash 'c 30 'a 100 'd 2))
    (hash 'a 100 'c 30 'd 2))

  (check-hash-equal?
    (hash-union (hash 'a 1 2 3 'c 'd) null)
    (hash 'a 1 2 3 'c 'd))

  (let ((e1 (hash
              'a (hash
                    'aa 10
                    'ab (hash
                          'aba -8
                          'abb -12))
              'b 20
              'e '(1 2 3)))
        (e2 (hash
              'a (hash
                    'aa 300
                    'ab (hash
                          'aba 1000
                          'abc -16)
                    'ac 400)
              'b (hash
                    'ba 33)
              'c 30
              'd 2
              'e '(3 4 5)))
        (e3 (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12
                            'abc -16)
                      'ac 400)
                'b 20
                'c 30
                'd 2
                'e '(1 2 3 3 4 5))))
    (check-hash-equal? (hash-fuse e1 e2) e3))

  (define h1 (hasher-by-names 'a 'b 'c))

  (check-hash-equal?
    (h1 8 12 14)
    (hash 'a 8 'b 12 'c 14))

  (check-hash-equal?
    (h1 8 12)
    (hash 'a 8 'b 12))

  (check-hash-equal?
    (h1 8 12 14 16)
    (hash 'a 8 'b 12 'c 14))

  (check-hash-equal?
    (hash-regex-filter (regexp "a.*a") (hash 'a 10 'b 20 'aba 30 'abba 40 "arda" 50 'cab 60))
    (hash 'aba 30 'abba 40 "arda" 50))

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

  (check-equal?
    (hash->ordered-list (hash 'b 20 'a 10 'c 30) '(a c b))
    '(10 30 20))

  (check-equal?
    (hash->ordered-list (hash 'b 20 "a" 10 8 17 'c 30) '("a" b c 8))
    '(10 20 30 17))

  (check-equal? (hash-length (hash 'a 10 'b '(1 2 3) 3 7)) 3)

  (check-equal?
    (hash-length (hash-take (hash 'a 1 'b 2 'c 3 'd 4 'e 5) 2))
    2)

  (check-hash-equal?
    (hash-group-by
      (hash
        1 (hash 'place "Оленёк" 'project "edu" 'budget 1000)
        2 (hash 'place "Якутск" 'project "tran" 'budget 3000)
        3 (hash 'place "Оленёк" 'project "tran" 'budget 2500)
        4 (hash 'place "Якутск" 'project "soc" 'budget 100)
        5 (hash 'place "Якутск" 'project "soc" 'budget 800))
      'place)
    (hash
      "Оленёк" (hash
                  1 (hash 'place "Оленёк" 'project "edu" 'budget 1000)
                  3 (hash 'place "Оленёк" 'project "tran" 'budget 2500))
      "Якутск" (hash
                  2 (hash 'place "Якутск" 'project "tran" 'budget 3000)
                  4 (hash 'place "Якутск" 'project "soc" 'budget 100)
                  5 (hash 'place "Якутск" 'project "soc" 'budget 800))))

  (check-hash-equal?
    (hash-remove-keys (hash 'a 10 'b 20 'c 30 'd 40 'e 50) '(a c d))
    (hash 'b 20 'e 50))

  (check-hash-equal? (init-hash (a b c)) (hash 'a "" 'b "" 'c ""))
  (check-hash-equal? (init-hash (a b c) 0) (hash 'a 0 'b 0 'c 0))

  ; (hash-class Figure (x y))
  ; (hash-class Circle Figure (r))
  ; (hash-class Rectangle Figure (w h))
  ; (hash-class Cube Rectangle (z))
  ;
  ; (define circle1 (Circle #:x 100 #:y 200 #:r 10))
  ; (define circle2 (Figure #:x 100 #:y 200))
  ; (define rectangle1 (Rectangle #:x 0 #:y 0 #:w 100 #:h 20))
  ;
  ; (check-equal? (hash-ref rectangle1 'w) 100)
  ; (check-equal? (hash-ref circle1 'r) 10)
  ; (check-equal? (hash-ref circle2 'r) "")

  (check-hash-equal?
    (tree->hash '(a (b (c d) e 10) f (g h)))
    (hash 'a (hash 'b '(c d) 'e 10) 'f '(g h)))

  (check-hash-equal?
    (tree->hash
      `(a
          (b (c d)
          ,(list 1 2) 100)
        e ,(list (hash 'aa 2) (hash 'bb 4 'cc 8))
        f (g h)))
    (hash
      'a (hash
            'b '(c d)
            (list 1 2) 100)
      'e (list (hash 'aa 2) (hash 'bb 4 'cc 8))
      'f '(g h)))

  (check-equal? (unify-hashes
                  (list (hash 'a 10 'b 20) (hash 'c 30 'd 40) (hash 'a 20 'e 40) (hash 'a 10 'c 30 'b 50 'f 80) (hash 'k 100 'm 200))
                  'a)
                (list (hash 'a 10 'b 20 'c 30 'f 80) (hash 'c 30 'd 40) (hash 'a 20 'e 40) (hash 'k 100 'm 200)))

  (check-hash-equal?
                (hash-keys-substitute (hash 'a 10 'b 20) '(a b) '(c d))
                (hash 'c 10 'd 20))

  (check-equal? (untyped-hash-ref (hash 'a 10 'b 20) 'b) 20)
  (check-equal? (untyped-hash-ref (hash 'a 10 'b 20) "b") 20)

  (check-equal? (format-hash "{a} - {b}" (hash 'a 10 'b 20)) "10 - 20")
  (check-equal? (format-hash "foo" (hash 'a 10 'b 20)) "foo")
  (check-equal? (format-hash "{a}/{c}/{a}" (hash 'a 10 'b 20 'c "baz")) "10/baz/10")

  (check-hash-equal?
                (hash-minus (hash 'a 10 'b 20 'c 30) (hash 'b 20 'd 40))
                (hash 'a 10 'c 30))

  (check-hash-equal?
                (hash-minus (hash 'a '(10 20) 'b 20 'c 30 'e '(1 2 3)) (hash 'a '(20 10) 'b 20 'd 40 'e '(1 3 2 4)) #:e same-elements?)
                (hash 'c 30 'e '(1 2 3)))

  )
