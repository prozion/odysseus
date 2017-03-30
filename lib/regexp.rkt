#lang racket

(require "base.rkt")
(require "seqs.rkt")

(provide (all-defined-out))

(define (re? x)
  (or (regexp? x) (pregexp? x)))

(define (->re x)
  (cond
    ((re? x) x)
    ((string? x) (pregexp x))
    (else (pregexp (str x)))))

(define (re-matches? re astr)
  (not (null? (regexp-match (->re re) astr))))

(define (get-matches re astr)
  (let ((re (->re re)))
    (for/fold
      ((res (list)))
      ((match-position (regexp-match-positions* re astr)))
      (pushr
        res
        (regexp-match
          re
          astr
          (car match-position))))))

(define (re-substitute astr re repstr)
  (string-replace astr (->re re) repstr))
