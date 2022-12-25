#lang racket

(require rackunit)
(require "../string.rkt")
(require "../debug.rkt")

(check-equal? (str "" "hello" " world!" "") "hello world!")
(check-equal? (str "1" (unless #t "2") "3") "13")
(check-equal? (str 123) "123")

(check-equal? (string-ref* "" 10 #:nth #t) #f)
(check-equal? (string-ref* "Oslo god morgen" 0 #:nth #t) #f)
(check-equal? (string-ref* "Oslo god morgen" 1 #:nth #t) "O")
(check-equal? (string-ref* "Oslo god morgen" 6 #:nth #t) "g")
(check-equal? (string-ref* "ost og skinke" -1 #:nth #t) "e")

(check-equal? (string-explode "") empty)
(check-equal? (string-explode " bake\nry") '(" " "b" "a" "k" "e" "\n" "r" "y"))

(check-equal? (dupstr "a" 5) "aaaaa")
(check-equal? (dupstr "foo " 3) "foo foo foo ")

(check-equal? (string-take "" 10) "")
(check-equal? (string-take "Black waters" 5) "Black")
(check-equal? (string-take "Black waters" 0) "")
(check-equal? (string-take "Black waters" 100) "Black waters")

(check-equal? (string-drop "" 10) "")
(check-equal? (string-drop "Oslo god morgen" 0) "Oslo god morgen")
(check-equal? (string-drop "Oslo god morgen") "slo god morgen")
(check-equal? (string-drop "Oslo god morgen" 10) "orgen")
(check-equal? (string-drop "Oslo god morgen" 100) "")
(check-equal? (string-drop "Oslo god morgen" -1) "n")
(check-equal? (string-drop "Oslo god morgen" -4) "rgen")

(check-equal? (string-index-of "" "e") #f)
(check-equal? (string-index-of "Hercules" "e") 1)
(check-equal? (string-index-of "Hercules" "a") #f)

(check-equal? (string-slice "" 1 3) "")
(check-equal? (string-slice "Oslo god morgen" 3 1) "")
(check-equal? (string-slice "Oslo god morgen" 2 7) "slo go")
(check-equal? (string-slice "Oslo god morgen" 4) "o god morgen")
(check-equal? (string-slice "Oslo god morgen" 4 100) "o god morgen")
(check-equal? (string-slice "Oslo god morgen" 4 -3) "o god morg")

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

(check-equal? (string-take-right "" 10) "")
(check-equal? (string-take-right "Black waters" 3) "ers")
(check-equal? (string-take-right "Black waters" 0) "")
(check-equal? (string-take-right "Black waters" 100) "Black waters")

(check-equal? (string-take-word "foo,bar" #:f second) "bar")
(check-equal? (string-take-word "foo;bar") "foo;bar")
(check-equal? (string-take-word "foo;bar" #:delimeter ";") "foo")
(check-equal? (string-take-word "foo;bar;baz" #:delimeter ";" #:f (λ (x) (string-append (second x) (third x)))) "barbaz")

(check-equal? (string-first-word "") "")
; (string-take-word "" (λ (x) (--- x) x) ",")
(check-equal? (string-first-word "foo,bar") "foo")

(check-equal? (string-splice
  "Tell me, of that ingenious hero" "O muse, " 9)
  "Tell me, O muse, of that ingenious hero")

(check-true (starts-with? "Hector" "H"))
(check-false (starts-with? "Hector" "h"))
