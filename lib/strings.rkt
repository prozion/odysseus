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

; defensive version of substring
(define-catch (substr txt start end)
  (if (> (string-length txt) end)
    (substring txt start end)
    txt))

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

(define (format-string format-str astr #:filler (filler ""))
  (define (format-string-iter format-str astr res-str)
      (cond
        ((empty? format-str) res-str)
        (else
          (let ((cur-f (car format-str))
                (cur-c (if (empty? astr)
                          filler
                          (car astr))))
            (cond
              ((equal? cur-f "c") (format-string-iter (cdr format-str) (if (empty? astr) empty (cdr astr)) (str res-str cur-c)))
              (else (format-string-iter (cdr format-str) astr (str res-str cur-f))))))))
(format-string-iter (split format-str) (split (->string astr)) ""))

(define-macro (when/str condition . expression)
  `(if ,condition (string-append ,@expression) ""))

(define (title-case? astr)
  (let ((astr (if (symbol? astr) (symbol->string astr) astr)))
    (re-matches? "^[A-ZА-Я].*" astr)))

(define-catch (titlefy astr)
  (string-append
    (string-upcase (nth astr 1))
    (triml astr)))

(define-catch (titlefy-if-sentence astr)
  (if (string-contains? astr " ")
    (titlefy astr)
    astr))

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

(define idfy (change-text
                  (list
                    (cons " " "_")
                    (cons "«" "")
                    (cons "»" "")
                    (cons "(" "")
                    (cons ")" "")
                    (cons "," "")
                    (cons ":" "")
                    (cons "—" "_")
                    (cons "-" "_")
                    (cons "/" "_")
                    (cons "ё" "е")
                    (cons "__" "_")
                    )))

(define namefy-nbsp (change-text
                  (list
                    (cons "_" "&nbsp;")
                    (cons "'" "\"")
                    (cons #px"(?<=\\S),(?=\\S)" ", "))))

(define textify (change-text
                  (list
                    (cons " - " " &ndash; ")
                    (cons "'" "\"")
                    (cons #px"(?<=\\S),(?=\\S)" ", "))))

(define linefy (change-text
                  (list
                    (cons "\t" " ")
                    (cons "\n" " ")
                    (cons "\r" ""))))

(define clean-newlines linefy)

(define (httpify txt (prefix "http"))
  (cond
    ((not txt) txt)
    ((empty-string? txt) txt)
    ((re-matches? "^https?://" txt) txt)
    ((re-matches? "^\\./" txt) txt)
    (else (str prefix "://" txt))))

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

(define (a-z a b) (string<? (string-downcase (->string a)) (string-downcase (->string b))))
(define (z-a a b) (string>? (string-downcase (->string a)) (string-downcase (->string b))))

(define-catch (take-one astr #:f (f car) #:delimeter (delimeter ","))
  (cond
    ((equal? astr "") "")
    (else
      (f (string-split (->string astr) delimeter)))))

(define-catch (take-last astr #:f (f (λ (x) (and (not-empty? x) (last x)))) #:delimeter (delimeter ","))
  (f (string-split (->string astr) delimeter)))

(define-catch (string-ltrim astr num)
  (cond
    ((not (string? astr)) astr)
    ((< (string-length astr) num) astr)
    (else (substring astr 0 num))))

(define (starts-with? astr one-char-string)
  (equal? (nth astr 1) one-char-string))

; "foobar" -> "fooobar"
; "foo bar" -> "\"foo bar\""
(define-catch (quotate-if-sentence astr)
  (cond
    ((re-matches? " " astr) (format "\"~a\"" astr))
    (else astr)))

(define-catch (remove-extra-whitespaces astr)
  (string-replace astr #px"\\s\\s+" " "))

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

(define-catch (string-car str)
  (cond
    ((empty-string? str) (error (format "строка ~a пуста" str)))
    (else (substring str 0 1))))

(define-catch (string-cdr str)
  (cond
    ((empty-string? str) (error (format "строка ~a пуста" str)))
    ((equal? (string-length str) 1) "")
    (else (substring str 1))))

(define-catch (cyr> a b)
  (cond
    ((empty-string? a) #f)
    ((empty-string? b) #t)
    (else
      (let* ((letters (explode "_абвгдеёжзийклмнопрстуфхцчшщьыъэюяabscdefghijklmnoprstuvwxyz0123456789"))
            (a (string-downcase a))
            (b (string-downcase b))
            (a-first (string-car a))
            (a-rest (string-cdr a))
            (b-first (string-car b))
            (b-rest (string-cdr b))
            (a-pos (indexof letters a-first))
            (b-pos (indexof letters b-first)))
        (cond
          ((equal? a-first b-first) (cyr> a-rest b-rest))
          (else (> a-pos b-pos)))))))

(define (cyr< a b)
  (not (cyr> a b)))

(module+ test

  (require rackunit)

  (check-equal? (dupstr "a" 5) "aaaaa")
  (check-equal? (dupstr "foo " 3) "foo foo foo ")

  ;(check-equal? (format-n "hello ~a" "world") "hello world")
  ;(check-equal? (format-n "hello ~l(,)" '("world" "verden")) "hello world, verden")

  (check-equal? (strnumber->number "3") 3)
  (check-= (strnumber->number "3,0") 3.0 1e-6)
  (check-= (strnumber->number "2 100,50") 2100.5 1e-6)

  (check-equal? (format-number "d ddd" 3504) "3 504")
  (check-equal? (format-number "ddd ddd" 38504) "38 504")
  (check-equal? (format-number "ddd" 38504) "38504")
  (check-equal? (format-number "d. d. d" 38504) "385. 0. 4")
  (check-equal? (format-number "dd" 3) "3")
  ; (check-equal? (format-number "dd" 3 #:filler "0") "03")
  (check-equal? (format-number "ddd" 3 #:filler "0") "003")

  (check-equal? (format-string "ccc ccc" "abcdef") "abc def")
  (check-equal? (format-string "ccc ccc" "hello") "hel lo")
  (check-equal? (format-string "cc" "hello") "he")
  (check-equal? (format-string "ccc ccc" "hello" #:filler "#") "hel lo#")
  (check-equal? (format-string "ccc-ccc" "" #:filler "#") "###-###")
  (check-equal? (format-string "cc ccc ccc-cc-cc" "+79054817655") "+7 905 481-76-55")

  (check-equal? (when/str (> 3 2) (format "~a ~a " "hello" 3) "world") "hello 3 world")
  (check-equal? (when/str (< 3 2) (format "~a ~a " "hello" 3) "world") "")

  (check-equal? (title-case? "A") #t)
  (check-equal? (title-case? "Abyss") #t)
  (check-equal? (title-case? "Зевс") #t)
  (check-equal? (title-case? "abyss") #f)
  (check-equal? (title-case? "m") #f)
  (check-equal? (title-case? "щиты") #f)

  (check-equal? (count-tabs "hello") 0)
  (check-equal? (count-tabs "\t\t\thello") 3)

  (check-equal? (word-sum "hello") 52)
  (check-equal? (word-sum "бhello") 54)
  (check-equal? (word-sum "абвгд") 15)

  (check-true (no-words? ""))
  (check-true (no-words? " "))
  (check-true (no-words? #f))
  (check-false (no-words? "s"))
  (check-false (no-words? "  s"))
  (check-false (no-words? "  a"))
  (check-false (no-words? "  s "))

  (check-equal? (sort-by-string '("a" b "cd" "c" "aa" 1 2)) '(1 2 "a" "aa" b "c" "cd"))
  (check-equal? (sort-by-string '("a" b "cd" "c" "aa" 1 2) 'a-z) '(1 2 "a" "aa" b "c" "cd"))
  (check-equal? (sort-by-string '("a" b "cd" "c" "aa" 1 2) 'z-a) (reverse '(1 2 "a" "aa" b "c" "cd")))

  (check-equal? (sort '("a" b "cd" "c" "aa" 1 2) a-z) '(1 2 "a" "aa" b "c" "cd"))
  (check-equal? (sort '("a" b "cd" "c" "aa" 1 2) a-z) '(1 2 "a" "aa" b "c" "cd"))
  (check-equal? (sort '("a" b "cd" "c" "aa" 1 2) z-a) (reverse '(1 2 "a" "aa" b "c" "cd")))

  (check-equal? (take-one "") "")
  (check-equal? (take-one "foo,bar") "foo")
  (check-equal? (take-one "foo,bar" #:f second) "bar")
  (check-equal? (take-one "foo;bar") "foo;bar")
  (check-equal? (take-one "foo;bar" #:delimeter ";") "foo")
  (check-equal? (take-one "foo;bar;baz" #:delimeter ";" #:f (λ (x) (string-append (second x) (third x)))) "barbaz")

  (check-true (starts-with? "Hector" "H"))
  (check-false (starts-with? "Hector" "h"))

  (check-true (cyr> "Питер" "Москва"))
  (check-true (cyr> "Washington" "Питер"))
  (check-true (cyr> "Самойлово" "Самара"))
  (check-true (cyr> "Самойлово" "самара"))
  (check-true (cyr> "Washington" "Boston"))
  (check-false (cyr> "Boston" "Seattle"))
)
