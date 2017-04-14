#lang racket

(require "seqs.rkt")

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

(define (str/escape astr)
  (let ((replace-syms '("\\" "\"")))
    (for/fold
      ((res astr))
      ((sym replace-syms))
      (string-replace astr sym (str "\\" sym)))))
