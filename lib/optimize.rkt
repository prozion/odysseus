#lang racket

(require (rename-in  racket/hash  (hash-union hash-union-racket)))
(require compatibility/defmacro)
(require "base.rkt")
(require "seqs.rkt")
(require "hash.rkt")

(provide (all-defined-out))

(define (memoize f)
  (let ((args-hash (hash)))
    (λ args
      (let* ((hashed (hash-ref args-hash args #f))
            (res (or hashed (apply f args))))
        (when (not hashed) (set! args-hash (hash-insert args-hash (cons args res))))
        res))))

(define (opt/uniques lst)
  (for/fold
    ((res empty))
    ((i lst))
    (if (member i res)
      res
      (rpush res i))))

(define (opt/uniques/unordered lst)
  (hash-keys
    (make-hash (map (λ (x) (cons x #t)) lst))))

(define (opt/implode lst (sep ""))
  (let* ((lst-length (length lst))
        (res-lst
          (for/fold
            ((s null))
            (
              (i (reverse lst))
              (c (in-range lst-length)))
            (cond
              ((= c (dec lst-length))
                (if (not (null? i))
                  (cons (format "~a" i) s)
                  s))
              ((null? i) (cons sep s))
              (else
                (cons sep (cons (format "~a" i) s)))
             ))))
    (apply string-append res-lst)))

(define (opt/exclude-all seq el)
  (cond
    ((string? seq)
        (string-replace seq el ""))
    (else (exclude-all seq el))))

(define (opt/split seq sep)
  (cond
    ((string? seq)
      (let ((seq (if (regexp-match ;; after fail to read the last empty field of _disabled in the googledoc table
                        (pregexp (format "[^~a]~a$" sep sep))
                        seq)
                    (string-append seq sep)
                    seq)))
        (string-split seq sep)))
    (else (split seq sep))))

(define (opt/difference seq1 seq2)
  (set-symmetric-difference seq2 (reverse seq1)))

(define (opt/append-unique . seqs)
  (opt/uniques (apply append seqs)))

(define (opt/hash-union . seqs)
  (apply hash-union-racket seqs))
