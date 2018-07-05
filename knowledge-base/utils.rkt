#lang racket

(require "../lib/load/all.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define (get-name item)
  (or ($ name item) ($ _name item) (namefy ($ id item))))

(define (prn param (prefix "") (postfix ""))
  (if (and param (non-empty-string? param))
    (str prefix (namefy param) postfix)
    ""))

(define (textify-interval-ru x)
  (let* ((dictionary
          (list
            (cons "-" " &ndash; ")
            (cons "<current>" "настоящее время"))))
    (for/fold
      ((res x))
      ((dict-pair dictionary))
      (string-replace res (car dict-pair) (cdr dict-pair)))))

(define (get-not-empty . args)
  (if (ormap non-empty-string? args)
    (first (filter-not empty-string? args))
    ""))

(define (get-first s)
  (first (string-split s ",")))

(define (get-folder-name id)
  (string-replace (string-downcase id) "_" "-"))

(define-macro (calculate expr)
  `(λ (x)
      (let ((x (->number x)))
          (->string ,expr))))
