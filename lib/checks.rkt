#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(define (check-hash-equal/simple? h1 h2)
  (equal?
    (sort
      (hash-values h1)
      <)
    (sort
      (hash-values h2)
      <)))

(define (check-hash h1 h2)
  (cond
    ((not (and (hash? h1) (hash? h2))) #f)
    (else
      (and
        (for/fold ((a #t)) (([k v2] h2))
            (let ((v1 (hash-ref h1 k #f)))
              (and  a
                    (if (hash? v1)
                      (check-hash v1 v2)
                      (equal? v1 v2)))))
        (for/fold ((a #t)) (([k v1] h1))
            (let ((v2 (hash-ref h2 k #f)))
              (and  a
                    (if (hash? v2)
                      (check-hash v1 v2)
                      (equal? v1 v2)))))))))

(define-macro (check-hash-equal? h1 h2)
  `(check-true
      (check-hash ,h1 ,h2)))
