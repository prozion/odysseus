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

(define (make-index items (index-name "i"))
  (for/fold
    ((res empty))
    ((item items) (idx (in-naturals 1)))
    (pushr
      res
      (hash-union
        (hash index-name idx)
        item))))

; {w,w<r,w>r:first-existed}
(define (first-existed (fallback-value #f))
  (λ args
    (let ((existed (filter-not nil? args)))
      (if (empty? existed)
        fallback-value
        (first existed)))))

(define (make-link id . urls)
  (let* ((name (namefy id)))
    (if (andmap nil? urls)
      name
      (format "<a href =\"~a\" target=\"_blank\">~a</a>"
              (httpify (for/or ((url urls)) url))
              name))))
