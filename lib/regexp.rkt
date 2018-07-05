#lang racket

(require "base.rkt")
(require "seqs.rkt" (for-syntax "seqs.rkt"))
(require compatibility/defmacro)

(provide (all-defined-out))

(define (re? x)
  (or (regexp? x) (pregexp? x)))

(define (->pre x)
  (cond
    ((re? x) x)
    ((string? x) (pregexp x))
    (else (pregexp (str x)))))

(define (->re x)
  (cond
    ((re? x) x)
    ((string? x) (regexp x))
    (else (regexp (str x)))))

(define (re-matches? re astr)
  (true? (regexp-match (->pre re) astr)))

(define (re-full-matches? re astr)
  (true? (re-matches? (str "^" re "$") astr)))

(define-catch (get-matches re astr)
  (let ((re (->re re)))
    (for/fold
      ((res (list)))
      ((match-position (regexp-match-positions* re astr)))
      ; BUG: gives an error, when parsing timeline.tree
      (begin
        ; (println (format "~a ~a ~a ~a" re astr match-position (regexp-match-positions* re astr)))
        (pushr
          res
          (regexp-match
            re
            astr
            (car match-position)))))) )

(define (get-first-group-match re astr)
  (let* ((res (get-matches re astr)))
    (match res
      (`((,_ ,g1)) g1)
      (else #f))))

(define (re-substitute astr re repstr)
  (cond
    ((and (list? re) (list? repstr))
      (if (or (null? (cdr re)) (null? (cdr repstr)))
        (re-substitute astr (car re) (car repstr))
        (re-substitute (re-substitute astr (car re) (car repstr)) (cdr re) (cdr repstr))))
    (else
      (string-replace astr (->pre re) repstr))))
