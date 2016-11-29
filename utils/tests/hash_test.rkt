#lang racket

(module+ test

  (require rackunit)
  (require "../checks.rkt")
  (require "../hash.rkt")

  (define h (hash 'a (hash 'aa 10 'ab 20) 'b (hash 'ba (hash 'baa 300 'bab 30))))

  (check-equal? (@ 'a 1 'b 2) (hash 'a 1 'b 2))
  (check-equal? (@ "" null #f empty 'a 1 'b 2) (hash 'a 1 'b 2))
  (check-equal? (@ (when #f 'a) 'b 10 'c 20) (hash 'b 10 'c 20))

  (check-equal? (hash-path (hash 'a 1 'b 10 'c 100) 'c) 100)
  (check-equal? (hash-path h 'b 'ba 'bab) 30)
  (check-false (hash-path h 'c 'ba 'baa))
  (check-false (hash-path h 'a 'ac))
  (check-false (hash-path h 'a 'ab 'aba))

  (check-equal? (@. h.a.aa) 10)
  (check-equal? (@. h.c) #f)
  (check-true
    (check-hash-equal?
      (@. h)
      h))

  (check-true
    (check-hash-equal?
      (hash-insert (hash 'a 10 'b 20) (cons 'c 30))
      (hash 'a 10 'b 20 'c 30)))

  ; TODO: how to compare hashes directly?
  (check-true
    (check-hash-equal?
      (hash-union (hash 'a 10 'b 20) (hash 'c 30 'a 100 'd 2))
      (hash 'a 10 'b 20 'c 30 'd 2)))

  (let ((e1 (hash-union (hash 'a (hash 'aa 10 'ab 20) 'b 20) (hash 'c 30 'a (hash 'aa 300 'ac 400) 'd 2)))
        (e2 (hash 'a (hash 'aa 10 'ab 20 'ac 400) 'b 20 'c 30 'd 2)))
    (check-true
      (check-hash-equal? e1 e2)))

  (let ((e3 (hash-union
              (hash
                'a (hash
                      'aa 10
                      'ab (hash
                            'aba -8
                            'abb -12))
                'b 20)
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
                'd 2)))
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
                'd 2)))
    (check-true
      (check-hash-equal? e3 e4)))

  (check-true
    (check-hash-equal?
      (hash-union null (hash 'c 30 'a 100 'd 2))
      (hash 'a 100 'c 30 'd 2)))

  (check-true
    (check-hash-equal?
      (hash-union (hash 'a 1 2 3 'c 'd) null)
      (hash 'a 1 2 3 'c 'd)))
)
