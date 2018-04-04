#lang racket

(module+ test

  (require rackunit)
  (require "../strings.rkt")

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
)
