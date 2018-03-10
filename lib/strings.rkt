#lang racket

(require "seqs.rkt")
(require "regexp.rkt")
(require compatibility/defmacro)

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
  (cond
    ((number? x) x)
    ((string? x)
      (let* (
            (res (re-substitute x '("," " ") '("." "")))
            (res (bytes->list (string->bytes/utf-8 res)))
            (res (exclude-all (exclude-all res 160) 194))
            (res (bytes->string/utf-8 (list->bytes res)))
            (res (string->number res))
            (res (if res res 0)))
        res))
    ((false? x) 0)
    (else x)))

(define (format-number format-str number #:filler (filler ""))
  (define (format-number-iter format-str number-str res-str)
      (cond
        ((null? number-str) (str
                              (dupstr filler (count-element format-str "d"))
                              res-str))
        ((null? format-str) (str (implode (reverse number-str)) res-str))
        (else
          (let ((cur-f (car format-str))
                (cur-d (car number-str)))
            (cond
              ((equal? cur-f "d") (format-number-iter (cdr format-str) (cdr number-str) (str cur-d res-str)))
              (else (format-number-iter (cdr format-str) number-str (str cur-f res-str))))))))
  (format-number-iter (reverse (split format-str)) (reverse (split (number->string number))) ""))

(define-macro (when/str condition . expression)
  `(if ,condition (string-append ,@expression) ""))

(define (title-case? astr)
  (let ((astr (if (symbol? astr) (symbol->string astr) astr)))
    (re-matches? "^[A-ZА-Я].*" astr)))

(define (mstring->string astr)
  (string-replace astr "\n" " "))

(define (count-tabs line (sym "\t"))
	(let loop ((count 0) (line (explode line)))
		(cond
			((equal? (car line) sym) (loop (+ 1 count) (cdr line)))
			(else count))))
