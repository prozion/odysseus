#lang racket

(require "../lib/all.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define-macro (sbgn/pd . args)
  `(let ((env (hash)) ,@args)))

(define-macro (simple-chemical chemical)
  `(quote ,chemical))

(define (take-element env element)
  (let ((cur-value (hash-ref en element 0)))
    (if (> cur-value 0)
      (hash-substitute env (cons element (dec cur-value)))
      env)))

(define (add-element env element)
  (let ((cur-value (hash-ref en element 0)))
      (hash-substitute env (cons element (inc cur-value)))))

(define (process ins modifiers outs)
  (let ((in-first (car ins))
        (out-first (car outs)))
      (add-element (take-element env in-first) out-first)))

(define (run-pd rules state-0)
  state-0)
