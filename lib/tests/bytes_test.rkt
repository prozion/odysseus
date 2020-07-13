#lang racket


(module+ test

  (require rackunit)
  (require "../bytes.rkt")
  (require "../seqs.rkt")

  (check-equal? (bytes-slice #"\100\0\2\1\20" 2 4) #"\0\2\1")

  ;;;;;;;;

  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0) #:char-mode 'big) (list 0 0 0 1 1 0 1 0))
  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0) #:char-mode 'little #:word-mode 'little) (list 0 1 0 1 1 0 0 0 1   1 0 0 1 0 1 0))
  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0) #:char-mode 'little #:word-mode 'big) (list 1 1 0 0 1 0 1 0   0 1 0 1 1 0 0 0))
  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0) #:char-mode 'big #:word-mode 'big) (list 0 1 0 1 0 0 1 1  0 0 0 1 1 0 1 0))
  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0) #:char-mode 'big #:word-mode 'little) (list 0 0 0 1 1 0 1 0  0 1 0 1 0 0 1 1))
  (check-equal? (bits->normal-bits (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0  1) #:char-mode 'big #:word-mode 'big) (list 1  0 1 0 1 0 0 1 1  0 0 0 1 1 0 1 0))

  (check-equal? (normal-bits->bits (list 0 1 0 1 0 0 1 1   0 0 0 1 1 0 1 0) #:char-mode 'big #:word-mode 'little) (list 1 1 0 0 1 0 1 0   0 1 0 1 1 0 0 0))
  (check-equal? (normal-bits->bits (list 0 1 0 1 0 0 1 1   0 0 0 1 1 0 1 0) #:char-mode 'big #:word-mode 'big) (list 0 1 0 1 1 0 0 0   1 1 0 0 1 0 1 0))
  (check-equal? (normal-bits->bits (list 0 1 0 1 0 0 1 1   0 0 0 1 1 0 1 0) #:char-mode 'little #:word-mode 'big) (list 0 0 0 1 1 0 1 0   0 1 0 1 0 0 1 1))
  (check-equal? (normal-bits->bits (list 0 1 0 1 0 0 1 1   0 0 0 1 1 0 1 0) #:char-mode 'little #:word-mode 'little) (list 0 1 0 1 0 0 1 1   0 0 0 1 1 0 1 0))

  ;;;;;;;;

  (check-equal? (char->normal-bits 34) (list 0 1 0 0 0 1 0 0))

  (check-equal? (number->normal-bits 8) (list 0 0 0 1))
  (check-equal? (number->normal-bits 8 #:bit-span 8) (list 0 0 0 1 0 0 0 0))

  (check-equal? (bytes->normal-bits (bytes 8 2)) (list 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0)) ; small-endian, small-ensian
  (check-equal? (bytes->normal-bits (bytes 8 2) #:word-mode 'little #:char-mode 'little) (list 0 0 0 1 0 0 0 0   0 1 0 0 0 0 0 0))
  (check-equal? (bytes->normal-bits (bytes 8 2) #:word-mode 'little #:char-mode 'big) (list 0 0 0 0 1 0 0 0   0 0 0 0 0 0 1 0))
  (check-equal? (bytes->normal-bits (bytes 8 2) #:word-mode 'big #:char-mode 'big) (list 0 0 0 0 0 0 1 0  0 0 0 0 1 0 0 0))
  (check-equal? (bytes->normal-bits (bytes 8 2) #:word-mode 'big #:char-mode 'little) (list 0 1 0 0 0 0 0 0  0 0 0 1 0 0 0 0))

  ;;;;;;;;;

  (check-equal? (normal-bits->char (list 0 1 0 0 0 0 0 0  0 0 0 1 0 0 0 0)) 2)
  (check-equal?
                (normal-bits->char (bits->normal-bits (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0) #:char-mode 'big))
                64)

  (check-equal? (normal-bits->integer (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0)) 2050)
  (check-equal? (normal-bits->integer
                  (bits->normal-bits (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0) #:word-mode 'little #:char-mode 'little))
                2050)
  (check-equal? (normal-bits->integer
                  (bits->normal-bits (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0) #:word-mode 'little #:char-mode 'big))
                4160)
  (check-equal?
                (normal-bits->integer
                  (bits->normal-bits (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0) #:word-mode 'big #:char-mode 'little))
                520)
  (check-equal?
                (normal-bits->integer
                  (bits->normal-bits (list 0 1 0 0 0 0 0 0   0 0 0 1 0 0 0 0) #:word-mode 'big #:char-mode 'big))
                16400)

  (check-=
                (normal-bits->double (reverse (list 0 1 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
                23.0
                1e-6)

  ;;;;;;;;;

  (check-equal? (bytes->integer #"\100\0\2\1") 16908352)
  (check-equal? (bytes->integer #"\100\0\2\1\120\5" #:integer-size 4) 16908352)
  (check-equal? (bytes->integer #"\100\0\2") 131136)
  (check-equal? (bytes->integer #"\100\0\2" #:word-mode 'little) 131136)
  (check-equal? (bytes->integer #"\100\0\2" #:word-mode 'big) 4194306)

  (check-= (bytes->double #:word-mode 'big #:char-mode 'big
                  (list->bytes
                    (map normal-bits->integer
                        '((0 1 0 0 0 0 0 0)
                          (0 0 1 1 0 1 1 1)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)))))
                23.0
                1e-6)

  (check-= (bytes->double #:word-mode 'big #:char-mode 'little
                  (list->bytes
                    (map normal-bits->integer
                      (map
                        reverse
                        '((0 1 0 0 0 0 0 0)
                          (0 0 1 1 0 1 1 1)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0))))))
                23.0
                1e-6)

  (check-= (bytes->double #:word-mode 'little #:char-mode 'big
                  (list->bytes
                    (map normal-bits->integer
                      (reverse
                        '((0 1 0 0 0 0 0 0)
                          (0 0 1 1 0 1 1 1)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0)
                          (0 0 0 0 0 0 0 0))))))
                23.0
                1e-6)

  (check-= (bytes->double #:word-mode 'little #:char-mode 'little
                  (list->bytes
                    (map normal-bits->integer
                      (reverse
                        (map
                          reverse
                          '((0 1 0 0 0 0 0 0)
                            (0 0 1 1 0 1 1 1)
                            (0 0 0 0 0 0 0 0)
                            (0 0 0 0 0 0 0 0)
                            (0 0 0 0 0 0 0 0)
                            (0 0 0 0 0 0 0 0)
                            (0 0 0 0 0 0 0 0)
                            (0 0 0 0 0 0 0 0)))))))
                23.0
                1e-6)

)
