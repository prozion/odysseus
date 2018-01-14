#lang racket

(require compatibility/defmacro)
(require "base.rkt")
(require "iter.rkt")
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
