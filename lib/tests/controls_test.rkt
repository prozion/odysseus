#lang racket

(module+ test

  (require rackunit)
  (require "../controls.rkt")
  (require "../math.rkt")

  (check-= ((-> sin sqrt) 4) (sin 2) 1e-6)

  (check-equal? ((~>
                    (λ (x y) (+ x y))
                    (λ args (list (length args) (apply + args))))
                  1 2 3 4 5)
                20)
  (check-equal? ((~>
                    (λ args (append args (list 10 20 30)))
                    (λ args (list (length args) (apply + args))))
                  1 2 3 4 5)
                '(5 15 10 20 30))
  (check-= ((~> sin avg) 10 2 45 6) 0 0.1)
  (check-= (apply (~> sin avg) '(10 2 45 6)) 0 0.1)

  (check-= (->> sin sqrt 4) (sin 2) 1e-6)
  (check-equal? (->> (λ (x) (filter odd? x)) (λ (x) (map (curry * 3) x)) (λ (x) (append '(-1 -2 -3) x)) '(1 2 3 4 5))
                '(-3 -9 3 9 15))

  (check-equal? (gen 1 5) '(1 1 1 1 1))

  (check-equal? (zor 3) 3)
  (check-equal? (zor 0 (- 2 2) (/ 0 5) 10 0) 10)

  (let ((x 10))
    (check-equal? (the x 10 100) 100))
  (let ((x 10))
    (check-equal? (the x 8 100) 0))

  (let ((x 10))
    (check-equal? (thenot x 10 100) 0))
  (let ((x 10))
    (check-equal? (thenot x 8 100) 100))

  (check-equal? (ifthe (λ (x) (> x 5)) 9 sqr sqrt) 81)
  (check-equal? (ifthe (λ (x) (< x 5)) 9 sqr sqrt) 3)

  (check-equal? (repeat-f append '(1 2 3) '((4) (5 (6)) (7))) '(1 2 3 4 5 (6) 7))
  (check-equal? (repeat-f + 0 '(1 2 3)) 6)
  (check-equal? (repeat-f + 0 '()) 0)
)
