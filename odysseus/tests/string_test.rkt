#lang racket

(require rackunit)
(require "../string.rkt")

(check-equal? (str "" "hello" " world!" "") "hello world!")
(check-equal? (str "1" (unless #t "2") "3") "13")
(check-equal? (str 123) "123")

(check-equal? (string-explode "") empty)
(check-equal? (string-explode " bake\nry") '(" " "b" "a" "k" "e" "\n" "r" "y"))

(check-equal? (dupstr "a" 5) "aaaaa")
(check-equal? (dupstr "foo " 3) "foo foo foo ")

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

(check-true (tabtree> "Питер" "Москва"))
(check-true (tabtree> "Washington" "Питер"))
(check-true (tabtree> "Самойлово" "Самара"))
(check-true (tabtree> "Самойлово" "самара"))
(check-true (tabtree> "Washington" "Boston"))
(check-false (tabtree< "Washington" "Boston"))
(check-false (tabtree> "Boston" "Seattle"))
