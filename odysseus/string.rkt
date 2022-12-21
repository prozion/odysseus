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

; extended version of substring
(define-catch (substring* txt start (end #f))
  (let* ((len (string-length txt))
        (start (if (negative? start) (+ len start 1) start))
        (end (if (not end) len end))
        (end (if (negative? end) (+ len end 1) end)))
    (if (< end len)
      (substring txt start end)
      (substring txt start))))

(define-catch (string-first s)
  (and
    (non-empty-string? s)
    (substring* s 0 1)))

(define-catch (string-last s)
  (and
    (non-empty-string? s)
    (substring* s -1)))

(define (string-rest s)
  (and
    (non-empty-string? s)
    (not (letter? s))
    (substring s 1)))

(define (string-explode s)
  (filter-not non-empty-string? (string-split s "")))

(define (str/escape s (escapees empty))
  (let ((replace-syms (merge (list "\\" "\"") escapees)))
    (for/fold
      ((res s))
      ((sym replace-syms))
      (string-replace s sym (str "\\" sym)))))

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
(format-string-iter (split format-str) (split (->string s)) ""))

(define-macro (when/str condition . expression)
  `(if ,condition (string-append ,@expression) ""))

(define (title-case? s)
  (let ((s (if (symbol? s) (symbol->string s) s)))
    (re-matches? "^[A-ZА-Я].*" s)))

(define-catch (titlefy s)
  (string-append
    (string-upcase (nth s 1))
    (triml s)))

(define-catch (titlefy-if-sentence s)
  (if (string-contains? s " ")
    (titlefy s)
    s))

(define (mstring->string s)
  (string-replace s "\n" " "))

(define (count-tabs line (sym "\t"))
	(let loop ((count 0) (line (string-explode line)))
		(cond
			((equal? (car line) sym) (loop (+ 1 count) (cdr line)))
			(else count))))

(define (word-sum word)
  (let ((en-letters (string-explode "abcdefghijklmnopqrstuvwxyz"))
        (ru-letters (string-explode "абвгдеёжзиклмнопрстуфхцчшщъыьэюя")))
    (for/fold
      ((res 0))
      ((w (string-explode word)))
      (+ res (indexof en-letters w) (indexof ru-letters w)))))

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

(define (empty-string? s)
  (or
    (not s)
    (equal? s "")))

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
    (indexof? cyr-letters letter)))

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

(define string-first string-first)

(define string-rest string-rest)

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

(define-catch (take-one s #:f (f car) #:delimeter (delimeter ","))
  (cond
    ((equal? s "") "")
    (else
      (f (string-split (->string s) delimeter)))))

(define-catch (take-last s #:f (f (λ (x) (and (not-empty? x) (last x)))) #:delimeter (delimeter ","))
  (f (string-split (->string s) delimeter)))

(define-catch (string-ltrim s num)
  (cond
    ((not (string? s)) s)
    ((< (string-length s) num) s)
    (else (substring s 0 num))))

(define (starts-with? s one-char-string)
  (equal? (nth s 1) one-char-string))

; "foobar" -> "fooobar"
; "foo bar" -> "\"foo bar\""
(define-catch (quotate-if-sentence s)
  (cond
    ((re-matches? " " s) (format "\"~a\"" s))
    (else s)))

(define-catch (remove-extra-whitespaces s)
  (string-replace s #px"\\s\\s+" " "))

; either a or b is a substring of the counterpart
(define-catch (symmetric-substring? a b)
  (let* (
        (a (string-downcase a))
        (b (string-downcase b))
        (a_length (string-length a))
        (b_length (string-length b)))
    (if (> a_length b_length)
      (string-contains? a b)
      (string-contains? b a))))

(define (string-reverse s)
  (cond (string? s)
    (implode (reverse (string-explode s)))
  (else (error (format "~a is not a string" s)))))

(define (string-sha s)
  (bytes->hex-string (sha1 (string->bytes/utf-8 s))))
