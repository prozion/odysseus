#lang racket

(require "list.rkt")
(require "type.rkt")
(require "base.rkt")
(require "regexp.rkt")
(require compatibility/defmacro)
(require sha)

(provide (all-defined-out))

(define str string-append)

(define (dupstr txt n)
  (implode
    (map
      (λ (x) txt)
      (range 0 n))))

(define not-empty-string? non-empty-string?)
(define empty-string? (negate non-empty-string?))

; extended version of substring
(define-catch (substring* txt start (end (string-length txt)))
  (let* ((len (string-length txt))
        (start (if (< start 0) (+ len start) start))
        (end (if (> end len) len end))
        (end (if (and end (< end 0)) (+ len end) end)))
    (cond
      ((empty-string? txt)
        txt)
      (else
        (substring txt start end)))))

(define (string-take s n)
  (substring* s 0 n))

(define (string-take-right s n)
  (let ((len (string-length s)))
    (substring* s (- len n))))

(define (string-drop s n)
  (substring* s n))

(define (string-drop-right s n)
  (let ((len (string-length s)))
    (substring* s 0 (- len n))))

(define (string-first s)
  (string-take s 1))

(define (string-last s)
  (string-take s -1))

(define (string-rest s)
  (string-drop s 1))

(define string-remove (curryr string-replace ""))

(define (string-explode s)
  (filter-not non-empty-string? (string-split s "")))

(define (string-splice s ss pos)
  (string-append (string-take s pos) ss (string-drop s pos)))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

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
  (format-number-iter (reverse (string-explode format-str)) (reverse (string-explode (->string number))) ""))

(define (format-string format-str s #:filler (filler ""))
  (define (format-string-iter format-str s res-str)
      (cond
        ((empty? format-str) res-str)
        (else
          (let ((cur-f (car format-str))
                (cur-c (if (empty? s)
                          filler
                          (car s))))
            (cond
              ((equal? cur-f "c") (format-string-iter (cdr format-str) (if (empty? s) empty (cdr s)) (str res-str cur-c)))
              (else (format-string-iter (cdr format-str) s (str res-str cur-f))))))))
(format-string-iter (string-explode format-str) (string-explode (->string s)) ""))

(define-macro (when/str condition . expression)
  `(if ,condition (string-append ,@expression) ""))

(define (title-case? s)
  (re-matches? "^[A-ZА-Я].*" s))

(define-catch (titlefy s)
  (string-append
    (string-upcase (string-first s))
    (string-drop s 1)))

(define (count-tabs line (sym "\t"))
	(let loop ((count 0) (line (string-explode line)))
		(cond
			((equal? (car line) sym) (loop (+ 1 count) (cdr line)))
			(else count))))

(define (word-sum word)
  (let ((en-letters (string-explode " abcdefghijklmnopqrstuvwxyz"))
        (ru-letters (string-explode " абвгдеёжзиклмнопрстуфхцчшщъыьэюя")))
    (for/fold
      ((res 0))
      ((w (string-explode word)))
      (+ res (index-of en-letters w) (index-of ru-letters w)))))

(define (no-words? s)
  (or
    (not s)
    (re-matches? "^\\s*$" s)))

; make text more correct and beautiful
(define-catch (change-text dictionary)
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

(define (letter? s)
  (= (string-length s) 1))

(define-catch (sort-by-string list-of-something (order 'a-z))
  (case order
    ((a-z) (sort
              list-of-something
              (λ (a b)
                (string<? (->string a) (->string b)))))
    ((z-a) (reverse (sort-by-string list-of-something 'a-z)))
    (else (error "unknown order parameter"))))

(define cyr-letters
  (let* ((cyr-letters "абвгдеёжзиклмнопрстуфхцчшщьыъэюя")
        (cyr-capital-letters (string-upcase cyr-letters))
        (cyr-letters (string-append cyr-capital-letters cyr-letters)))
    (string-explode cyr-letters)))

(define-catch (cyr? letter)
  (and
    (letter? letter)
    (index-of cyr-letters letter)))

(define (а-яa-z a b)
  (let* (
        (a (string-downcase (->string a)))
        (b (string-downcase (->string b)))
        (a1 (string-first a))
        (b1 (string-first b)))
    (cond
      ; cyrrilic goes before latin:
      ((and (cyr? a1) (not (cyr? b1))) #t)
      ((and (not (cyr? a1)) (cyr? b1)) #f)
      ((and (cyr? a1) (cyr? b1))
        (cond
          ((string>? a b) #f)
          ((string<? a b) #t)
          (else
            (а-яa-z (string-rest a) (string-rest b)))))
      (else (string<? a b)))))

(define (a-z a b) (string<? (string-downcase (->string a)) (string-downcase (->string b))))
(define (z-a a b) (string>? (string-downcase (->string a)) (string-downcase (->string b))))
(define (A-Za-z a b)
  (let* ((a-capital? (title-case? a))
        (b-capital? (title-case? b)))
    (cond
      ((or
        (and a-capital? b-capital?)
        (and (not a-capital?) (not b-capital?)))
          (string<? (string-downcase (->string a)) (string-downcase (->string b))))
      ((and a-capital? (not b-capital?)) #t)
      ((and (not a-capital?) b-capital?) #f))))

(define-catch (0-9 a b)
  (< (->number a) (->number b)))

(define-catch (string-take-word s f delimeter)
  (if (empty-string? s "")
    s
    (f (string-split s delimeter))))

(define (string-first-word s (delimeter ","))
  (string-take-word s first delimeter))

(define (string-last-word s (delimeter ","))
  (string-take-word s last delimeter))

(define starts-with? string-prefix?)

(define (string-sha s)
  (bytes->hex-string (sha1 (string->bytes/utf-8 s))))

(define (random-word size #:prefix (prefix ""))
  (let* ((letters "abcdefghijklmnopqrstuvwxyz")
        (letters (map string (string->list letters))))
    (define (random-word-iter size result)
      (if (<= size 0)
        result
        (random-word-iter (sub1 size) (string-append result (list-ref letters (random (length letters)))))))
    (string-append prefix (random-word-iter size ""))))
