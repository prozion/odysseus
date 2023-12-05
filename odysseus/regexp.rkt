#lang racket

(require "base.rkt")
(require "list.rkt" (for-syntax "list.rkt"))
(require compatibility/defmacro)

(provide (all-defined-out))

(define (re? x)
  (or (regexp? x) (pregexp? x)))

(define (->pre x)
  (cond
    ((re? x) x)
    ((string? x) (pregexp x))
    (else (pregexp (~a x)))))

(define (->re x)
  (cond
    ((re? x) x)
    ((string? x) (regexp x))
    (else (regexp (~a x)))))

(define (re-matches? re astr)
  (true? (regexp-match (->pre re) astr)))

(define (re-full-matches? re astr)
  (true? (re-matches? (~a "^" re "$") astr)))

(define-catch (get-matches re astr (->mode ->re))
  (let ((re (->mode re)))
    (for/fold
      ((res (list)))
      ((match-position (regexp-match-positions* re astr)))
      ; BUG: gives an error, when parsing timeline.tree
      (let*
          ((next-match (regexp-match re astr (car match-position))))
          ; (_ (display (format "re: ~a\nastr: ~a\n(regexp-match ...): ~a\nmatch-position: ~a\n\n" re astr next-match match-position))))
        (if next-match
          (pushr res next-match)
          res)))))

(define (get-matches-pre re astr)
  (get-matches re astr ->pre))

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

(define-catch (regular-expression-string? astr)
  (or
    (string-contains? astr "\\")
    (string-contains? astr ".")
    (string-contains? astr "+?")
    (string-contains? astr "*?")
    (string-contains? astr "]*")
    (string-contains? astr "|")
    (string-contains? astr "]+")))

; regular expression string (rs)
(define (rs . args)
  (pregexp
    (format "^~a$" (apply string-append args))))

(define (re->string re)
  (object-name re))

(define (re-format frmt . args)
  (let* ((any-pregexps? (ormap pregexp? args))
        (any-regexps? (ormap regexp? args))
        (f (if any-pregexps? pregexp regexp))
        (->str (λ (x) (if (string? x) x (re->string x)))))
    (f (apply format frmt (map ->str args)))))
