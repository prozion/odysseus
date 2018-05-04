#lang racket

(module+ test

  (require rackunit)
  (require "../checks.rkt")
  (require "../hashtree.rkt")
  (require "../type.rkt")
  (require "../debug.rkt")

(define hash-tree-1 (hash
                      (hash 'id "category 1")
                        (hash
                          (hash 'id "a" 'value "1")
                            (hash)
                          (hash 'id "b" 'value "2")
                            (hash
                              (hash 'id "b1" 'value "10")
                              (hash))
                          (hash 'id "c" 'value "3")
                            (hash))
                      (hash 'id "category 2" 'status "inactive")
                        (hash
                          (hash 'id "d" 'value "-1")
                            (hash) )
                      ))

(define hash-tree-2 (hash
                      (hash 'id "root1")
                        (hash
                          (hash 'id "a" 'value "1")
                            (hash)
                          (hash 'id "b" 'value "2")
                            (hash
                              (hash 'id "b1" 'value "10")
                              (hash)
                              (hash 'id "b2" 'value "30")
                              (hash))
                          (hash 'id "c" 'value "3")
                            (hash))
                      (hash 'id "root2" 'status "inactive")
                        (hash
                          (hash 'id "d" 'value "-1")
                            (hash) )
                      ))

  (check-true (category? (hash 'id "category 1")))
  (check-true (category? (hash 'id "category 1" '_parent #f)))
  (check-false (category? (hash 'id "category 1" 'foo "bar")))
  (check-false (category? (hash 'id "c" 'value "3")))

  (check-same-elements? (planarize hash-tree-1)
                        (list
                          (hash 'id "category 1")
                          (hash 'id "a" 'value "1")
                          (hash 'id "b" 'value "2")
                          (hash 'id "b1" 'value "10")
                          (hash 'id "c" 'value "3")
                          (hash 'id "category 2" 'status "inactive")
                          (hash 'id "d" 'value "-1")))


  (check-equal? (hash-tree-get-value
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2 b21))
                      40)

  (check-equal? (hash-tree-get-value-by-id-path
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2 b21))
                      #f)

  (check-equal? (hash-tree-get-value-by-id-path
                        (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                              (hash 'id 'y) (hash
                                              (hash 'id 'z 'foo 0) 30
                                              'b2 (hash 'b21 40 'b22 50)))
                        '(y z))
                      30)

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "b" "b1"))
                      (hash))

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "b"))
                      (hash
                        (hash 'id "b1" 'value "10")
                        (hash)))

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "a"))
                      (hash))

  (check-hash-equal? (hash-tree-set-value
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2)
                        100)
                      (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 100)))

  (check-hash-equal?
                    (hash-tree-set-value-by-id-path
                        (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                              (hash 'id 'y) (hash
                                              (hash 'id 'z 'foo 0) 30
                                              'b2 (hash 'b21 40 'b22 50)))
                        '(y z)
                        200)
                    (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                          (hash 'id 'y) (hash
                                          (hash 'id 'z 'foo 0) 200
                                          'b2 (hash 'b21 40 'b22 50))))

  (check-same-elements?
                (get-leaves hash-tree-1)
                (list
                  (hash 'id "a" 'value "1")
                  (hash 'id "b" 'value "2")
                  (hash 'id "b1" 'value "10")
                  (hash 'id "c" 'value "3")
                  (hash 'id "category 2" 'status "inactive")
                  (hash 'id "d" 'value "-1")))

  (check-same-elements?
                (get-leaves hash-tree-1 #:exclude '(status))
                (list
                  (hash 'id "a" 'value "1")
                  (hash 'id "b" 'value "2")
                  (hash 'id "b1" 'value "10")
                  (hash 'id "c" 'value "3")
                  (hash 'id "d" 'value "-1")))

  (check-hash-equal?
    (get-item-by-id-from-the-list
      (list (hash 'id "a") (hash 'id "b" 'value "1") (hash 'id "c" 'value "2"))
      "c")
    (hash 'id "c" 'value "2"))

  (check-same-elements?
    (get-paths hash-tree-1)
    '(("category 1" "a") ("category 1" "b" "b1") ("category 1" "c") ("category 2" "d")))

  (check-hash-equal? ($$ root1.b.b1 hash-tree-2) (hash 'id "b1" 'value "10"))

  (check-hash-equal?
    ($$$ root1.b hash-tree-2)
    (hash
      (hash 'id "b1" 'value "10")
      (hash)
      (hash 'id "b2" 'value "30")
      (hash)))

  (check-hash-equal? ($$$ root1.b.b1 hash-tree-2) (hash))
  ; (check-equal? ($$$ root1.b.b1.value hash-tree-2) "10")

)
