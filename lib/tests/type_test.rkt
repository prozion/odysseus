#lang racket

(module+ test

  (require rackunit)
  (require "../type.rkt")

  (check-pred cons? (cons 1 2))
  (check-equal? (cons? (list 1 2)) #f)

  (check-pred list2? '(()))
  (check-pred list2? '((1)))
  (check-pred list2? '((1 2 3 4)))
  (check-pred list2? '((1 2 3 4) (3 4 5) (8 9)))
  (check-pred list2? '(((9))))
  (check-pred list2? '((1 2 3 4) ((3 4 5) (8 9))))

  (check-false (list2? '((1 2 3 4) (3 4 5) 10 (8 9))))

  (check-pred list-of-cons? (list (cons 1 2) (cons 4 5)))
  (check-equal? (list-of-cons? (list (cons 1 2) (cons 4 5) 10)) #f)

  (check-equal? (type? 3) 'number)
  (check-equal? (type? "a str") 'string)
  (check-equal? (type? #"a str") 'bytes)
  (check-equal? (type? '((a 10) (b 20))) 'alist)
  (check-equal? (type? (list (cons 1 2) (cons 3 4))) 'list-of-cons)
  (check-equal? (type? '((a 10) (b 20 30))) 'list2)
  (check-equal? (type? '(1 2 3)) 'list)
  (check-equal? (type? (cons 1 2)) 'pair)
  (check-equal? (type? #\a) 'char)
  (check-equal? (type? #\λ) 'char)
  (check-equal? (type? #\u0011) 'char)
  (check-equal? (type? 'a) 'symbol)
  (check-equal? (type? (λ (x) x)) 'procedure)
  (check-equal? (type? odd?) 'procedure)
  (check-equal? (type? #'(+ 1 2)) 'syntax)
  (check-equal? (type? #(1 2 3)) 'vector)
  (check-equal? (type? (hash 'a 10 'b 20)) 'hash)
  (check-equal? (type? (current-directory)) 'path)
)
