#lang racket

(require compatibility/defmacro)
(require "base.rkt")
(require "seqs.rkt")
(require "controls.rkt")

(provide (all-defined-out))

(define (bytes-slice b start end)
  (list->bytes (slice (bytes->list b) start end)))

;;;;;;;;

(define (bits->normal-bits b #:char-mode (char-mode 'little) #:word-mode (word-mode 'little) #:byte-size (byte-size 8))
  (let* ((chunks (partition-all b byte-size))
        (chunks (if (equal? char-mode 'big) (map reverse chunks) chunks))
        (chunks (if (equal? word-mode 'big) (reverse chunks) chunks)))
    (apply merge chunks)))

(define (normal-bits->bits b #:char-mode (char-mode 'little) #:word-mode (word-mode 'little) #:byte-size (byte-size 8))
  (bits->normal-bits b #:char-mode char-mode #:word-mode word-mode #:byte-size byte-size))

;;;;;;;;

(define (char->normal-bits ch)
  (for/fold
    ((s (list)))
    ((i (range 8)))
    (let* ( (nextbit
              (bitwise-and ch (arithmetic-shift 1 i)))
            (nextbit (if (> nextbit 0) 1 0)))
        (pushr s nextbit))))

(define (number->normal-bits d #:bit-span (bit-span #f))
  (define (fi d res)
    (cond
      ((= d 0) res)
      (else (fi (quotient d 2) (pushr res (remainder d 2))))))
  (let ((r (fi d (list))))
    (if (and bit-span (< (length r) bit-span))
          (merge r (gen 0 (- bit-span (length r))))
          r)))

(define (bytes->normal-bits b #:word-mode (word-mode 'little) #:char-mode (char-mode 'little))
  (let* ( (op1
              (map
                (λ (x)
                  (if (equal? char-mode 'big)
                    (reverse (char->normal-bits x))
                    (char->normal-bits x)))
                (bytes->list b)))
          (op2 (if (equal? word-mode 'big) (reverse op1) op1)))
    (apply merge op2)))

;;;;;;;;

(define (normal-bits->char b)
  (let ((b (if (> (length b) 8) (slice b 1 8) b)))
    (normal-bits->integer b)))

(define (normal-bits->integer b)
    (for/fold/idx (s 0) (i b) (+ s (* i (expt 2 $idx)))))

(define (normal-bits->double b)
  (let*
    (
    (b (reverse b))
    (signed-bit (car b))
    (sign (if (> signed-bit 0) -1 1))
    (exponent (slice b 2 12))
    (exponent (normal-bits->bits exponent #:word-mode 'big #:char-mode 'big))
    (e (normal-bits->integer exponent))
    (e (- e 1023))
    (fraction (slice b 13 64))
)
    (* sign
      (for/fold/idx (s 1) (i fraction) (+ s (* i (expt 2.0 (- 0 $idx 1)))))
      (expt 2.0 e))
))

;;;;;;;;

(define (bytes->integer bstr #:word-mode (word-mode 'little) #:char-mode (char-mode 'little) #:integer-size (integer-size 4))
  (let ((bstr (if (> (bytes-length bstr) integer-size)
                      (bytes-slice bstr 1 integer-size)
                      bstr)))
    (normal-bits->integer
      (bytes->normal-bits bstr #:word-mode word-mode #:char-mode char-mode))))

(define (bytes->double bstr #:word-mode (word-mode 'little) #:char-mode (char-mode 'little) #:double-size (double-size 8))
  (let ((bstr (if (> (bytes-length bstr) double-size)
                      (bytes-slice bstr 1 double-size)
                      bstr)))
    (normal-bits->double
      (bytes->normal-bits bstr #:word-mode word-mode #:char-mode char-mode))))

(module+ test

  (require rackunit)
  (require "seqs.rkt")

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
