#lang racket

(require "seqs.rkt")
(require "type.rkt")
(require "base.rkt")
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
        ((null? number-str)
                            (str
                              (dupstr filler (count-element format-str "d"))
                              res-str))
        ((null? format-str) (str (implode (reverse number-str)) res-str))
        (else
          (let ((cur-f (car format-str))
                (cur-d (car number-str)))
            (cond
              ((equal? cur-f "d") (format-number-iter (cdr format-str) (cdr number-str) (str cur-d res-str)))
              (else (format-number-iter (cdr format-str) number-str (str cur-f res-str))))))))
  (format-number-iter (reverse (split format-str)) (reverse (split (->string number))) ""))

(define-macro (when/str condition . expression)
  `(if ,condition (string-append ,@expression) ""))

(define (title-case? astr)
  (let ((astr (if (symbol? astr) (symbol->string astr) astr)))
    (re-matches? "^[A-ZА-Я].*" astr)))

(define-catch (titlefy astr #:only-first (only-first #f))
  (cond
    ((not astr) astr)
    ((equal? astr "") astr)
    (only-first
      (let* (
            (words (string-replace astr "_" " "))
            (words (split words " "))
            (first-word (car words))
            (rest-words (cdr words))
            (first-word (string-titlecase first-word))
            (words (pushl rest-words first-word))
            (result (implode words " ")))
        result))
    (else
      (string-titlecase (string-replace astr "_" " ")))))

(define (mstring->string astr)
  (string-replace astr "\n" " "))

(define (count-tabs line (sym "\t"))
	(let loop ((count 0) (line (explode line)))
		(cond
			((equal? (car line) sym) (loop (+ 1 count) (cdr line)))
			(else count))))

(define (word-sum word)
  (let ((en-letters (explode "abcdefghijklmnopqrstuvwxyz"))
        (ru-letters (explode "абвгдеёжзиклмнопрстуфхцчшщъыьэюя")))
    (for/fold
      ((res 0))
      ((w (explode word)))
      (+ res (indexof en-letters w) (indexof ru-letters w)))))

(define (no-words? astr)
  (or
    (not astr)
    (re-matches? "^\\s*$" astr)))

; make text more correct and beautiful
(define (change-text dictionary)
  (λ (txt)
    (for/fold
      ((res (->string txt)))
      ((s dictionary))
      (string-replace res (car s) (cdr s)))))

(define namefy (change-text
                  (list
                    (cons "_" " ")
                    (cons "'" "\"")
                    (cons #px"(?<=\\S),(?=\\S)" ", "))))

(define textify (change-text
                  (list
                    (cons " - " " &ndash; ")
                    (cons "'" "\"")
                    (cons #px"(?<=\\S),(?=\\S)" ", "))))

(define (httpify txt)
  (cond
    ((not txt) txt)
    ((re-matches? "^https?://" txt) txt)
    ((re-matches? "^\\./" txt) txt)
    (else (str "http://" txt))))

; used for comparing messages in vk
(define simplify-text
  (change-text
    (list
      (cons "." " ")
      (cons "—" "-")
      (cons "\n" " ")
      (cons #px" $" "")
      (cons #px"\\s*-\\s*" "-")
      (cons #px"\\s{2,}" " ")
      (cons #px"[\\.,;:\\\\/\\!?()\\[\\]]" ""))))

(define (empty-string? astr)
  (or
    (not astr)
    (equal? astr "")))

(define-catch (sort-by-string list-of-something (order 'a-z))
  (case order
    ((a-z) (sort
              list-of-something
              (λ (a b)
                (string<? (->string a) (->string b)))))
    ((z-a) (reverse (sort-by-string list-of-something 'a-z)))
    (else (error "unknown order parameter"))))
