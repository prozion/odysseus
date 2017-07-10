#lang racket

(module+ test

  (require rackunit)
  (require "../alist.rkt")
  (require "../test.rkt")
  (require "../debug.rkt")

  (check-pred alist? '((a 10) (b 20)))
  (check-pred alist? '((a 10)))
  (check-pred alist? '(((1 2) 10)))
  (check-pred alist? '((null null)))
  (check-false (alist? 3))
  (check-false (alist? 'a))
  (check-false (alist? '(3)))
  (check-false (alist? '(3 4)))
  (check-false (alist? '((a 10) (b 2 3))))

  (check-equal? (firsts '((a 2) (b 10))) '(a b))

  (check-equal? (seconds '((a 2) (b 10))) '(2 10))

  (check-speed
              (alist-expand (dup-alist (1 2 3) 100) (range 1 1000) 50)
              10)

  (check-equal? (clist-add '((1 . 2) (3 . 4))
                            '(3 . 5)
                            (λ (v1 v2) (+ v1 v2)))
                '((1 . 2) (3 . 9)))

  (check-equal? (clist-add '((1 . 2) (3 . 4))
                            '(4 . 5)
                            (λ (v1 v2) (+ v1 v2)))
                '((1 . 2) (3 . 4) (4 . 5)))

  (check-equal? (clist-sort
                  '((1 . 3) (2 . 2) (3 . 3) (7 . 1) (4 . 2) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1))
                  (λ (k1 v1 k2 v2) (< k1 k2)))
                '((1 . 3) (2 . 2) (3 . 3) (4 . 2) (5 . 1) (7 . 1) (8 . 1) (9 . 1) (10 . 1) (44 . 1) (45 . 1)))

  (check-equal? (clist-sort
                  '((1 . 3) (2 . 2) (3 . 3) (7 . 1) (4 . 2) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1))
                  (λ (k1 v1 k2 v2) (< v1 v2)))
                '((7 . 1) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1) (2 . 2) (4 . 2) (1 . 3) (3 . 3)))

  (check-equal? (pairwise '(1 2 3) '(4 5 6))
                '((1 . 4) (2 . 5) (3 . 6)))

  (check-equal? (pairwise '(1 2 3) '(4 5 6 7))
                '((1 . 4) (2 . 5) (3 . 6)))

  (check-equal? (pairwise '(1 2 3 10) '(4 5 6))
                '((1 . 4) (2 . 5) (3 . 6)))

)
