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

  (check-true
    (check-hash-equal?
      (hash-insert (hash 'a 10 'b 20) (cons 'c 30))
      (hash 'a 10 'b 20 'c 30)))

  ; TODO: how to compare hashes directly?
  (check-true
    (check-hash-equal?
      (hash-union (hash 'a 10 'b 20) (hash 'c 30 'a 100 'd 2))
      (hash 'a 10 'b 20 'c 30 'd 2)))

  (check-true
    (check-hash-equal?
      (hash-union null (hash 'c 30 'a 100 'd 2))
      (hash 'a 100 'c 30 'd 2)))

  (check-true
    (check-hash-equal?
      (hash-union (hash 'a 1 2 3 'c 'd) null)
      (hash 'a 1 2 3 'c 'd)))
)
