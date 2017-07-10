#lang racket

(require "seqs.rkt")
(require "regexp.rkt")

(provide (all-defined-out))

(define (dupstr txt n)
  (implode
    (map
      (λ (x) txt)
      (range 0 n))))

;(define (format-n template seq)
;  (cond
;    ((list? seq) (format-n (re-substitute template "~l\((.*?)\)")
;    ((hash? seq) template)
;    (else (format (re-substitute template '("~(l|h)") '("~a")) seq))))

(define (str/escape astr (escapees empty))
  (let ((replace-syms (merge (list "\\" "\"") escapees)))
    (for/fold
      ((res astr))
      ((sym replace-syms))
      (string-replace astr sym (str "\\" sym)))))

(define (strnumber->number x)
  (let* ((res (re-substitute x '("," " ") '("." "")))
        (res (bytes->list (string->bytes/utf-8 res)))
        (res (exclude-all (exclude-all res 160) 194))
        (res (bytes->string/utf-8 (list->bytes res)))
        (res (string->number res)))
    res))
