#lang racket

(module+ test

  (require rackunit)
  (require "../seqs.rkt")

  (check-equal? (len "") 0)
  (check-equal? (len "Oslo is a capital of Great Britain") 34)
  (check-equal? (len '()) 0)
  (check-equal? (len '(1 2 3 4 5)) 5)

  (check-equal? (tostring '(1 2 3)) "(1 2 3)")

  (check-equal? (reverse "Lestrigons") "snogirtseL")
  (check-equal? (reverse '(1 2 3 4 5)) '(5 4 3 2 1))

  (check-equal? (str "" "hello" " world!" "") "hello world!")
  (check-equal? (str "1" (unless #t "2") "3") "13")

  (check-equal? (implode '("a" "b" "c" "d") "-") "a-b-c-d")
  (check-equal? (implode '("long" "sail" "across" "the" "sea") " ") "long sail across the sea")

  (check-equal? (implode empty) "")
  (check-equal? (implode '(" " "b" "a" "k" "e" "\n" "r" "y")) " bake\nry")
  (check-equal? (implode '(1 2 3 4)) "1234")
  (check-equal? (implode '(1 2 3 4) "+") "1+2+3+4")
  (check-equal? (implode (list null null null) ",") ",,")

  (check-equal? (intermix 0 '(1 2 3 4)) '(1 0 2 0 3 0 4))
  (check-equal? (intermix "." "Orest") "O.r.e.s.t")

  (check-equal? (interleave '(1 2) '(3 4)) '(1 3 2 4))
  (check-equal? (interleave '(1 2 10) '(3 4)) '(1 3 2 4))
  (check-equal? (interleave '(1 2) '(3 4 10)) '(1 3 2 4))

  (check-equal? (explode "") empty)
  (check-equal? (explode " bake\nry") '(" " "b" "a" "k" "e" "\n" "r" "y"))

  (check-equal? (split "") '())
  (check-equal? (split "Oslo") '("O" "s" "l" "o"))
  (check-equal? (split "Dar-as-Salam" "-") '("Dar" "as" "Salam"))
  (check-equal? (split '(1 2 0 3 4 0 5 0 6) 0) '((1 2) (3 4) (5) (6)))

  (check-equal? (nth "" 10) "")
  (check-equal? (nth "Oslo god morgen" 0) null)
  (check-equal? (nth "Oslo god morgen" 1) "O")
  (check-equal? (nth "Oslo god morgen" 6) "g")
  (check-equal? (nth "ost og skinke" -1) "e")
  (check-equal? (nth '() 10) null)
  (check-equal? (nth '(0 1 2 3 4 5) 0) null)
  (check-equal? (nth '(0 1 2 3 4 5) 1) 0)
  (check-equal? (nth '(0 1 2 3 4 5) 3) 2)
  (check-equal? (nth '(0 1 2 3 4 5) -1) 5)
  (check-equal? (nth '(0 1 2 3 4 5) 7) null)

  (check-equal? (nth-cycled '(0 1 2 3 4 5) 7) 0)
  (check-equal? (nth-cycled '(0 1 2 3 4 5) 17) 4)
  (check-equal? (nth-cycled '(1 2 3 4 5) 10) 5)

  (check-equal? (indexof "" "e") 0)
  (check-equal? (indexof "Hercules" "e") 2)
  (check-equal? (indexof "Hercules" "a") 0)
  (check-equal? (indexof '(11 -22 30 80 -5) 30) 3)
  (check-equal? (indexof '(11 -22 30 80 -5) -5) 5)
  (check-equal? (indexof '(11 -22 30 80 -5) 333) 0)

  (check-true (indexof? '(a b c d e f) 'd))
  (check-true (indexof? '(1 (10 1) 2) '(10 1)))
  (check-false (indexof? '(1 2 3 4 5) 6))
  (check-true (indexof? "abcdef" "d"))
  (check-false (indexof? "abcdef" "k"))

  (check-equal? (indexof-all "Hercules" "e") '(2 7))
  (check-equal? (indexof-all '(11 8 -22  8 30 80 -5 8) 8) '(2 4 8))

  (check-true (regexp-indexof? '("doo" "fowl" "island") "doo"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "i.*d"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "do{2}"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "f[oae]wl"))
  (check-false (regexp-indexof? '("doo" "fowl" "island") "baz"))
  (check-false (regexp-indexof? '("doo" "fowl" "island") "f[auy]+wl"))

  (check-equal? (lshift "" 10) "")
  (check-equal? (lshift "Black waters" 5) "Black")
  (check-equal? (lshift "Black waters" 0) "")
  (check-equal? (lshift "Black waters" 100) "Black waters")
  (check-equal? (lshift '(1 2 3 4 5 6 7 8 9)) '(1))
  (check-equal? (lshift '(1 2 3 4 5 6 7 8 9) 2) '(1 2))

  (check-equal? (lpop '(1 2 3 4 5 6 7 8 9)) 1)
  (check-equal? (first "Andromachus") "A")

  (check-equal? (ltrim "" 10) "")
  (check-equal? (ltrim "Oslo god morgen" 0) "Oslo god morgen")
  (check-equal? (ltrim "Oslo god morgen") "slo god morgen")
  (check-equal? (ltrim "Oslo god morgen" 10) "orgen")
  (check-equal? (ltrim "Oslo god morgen" 100) "")
  (check-equal? (ltrim "Oslo god morgen" -5) "") ; add contract!
  (check-equal? (ltrim '(1 2 3 4 5 6 7 8 9) 2) '(3 4 5 6 7 8 9))

  (check-equal? (lpush '() 100) '(100))
  (check-equal? (lpush '(1 2 3) 100) '(100 1 2 3))

  (check-equal? (lpush-unique '(1 2 3) 100) '(100 1 2 3))
  (check-equal? (lpush-unique '(1 2 3) 3) '(1 2 3))

  (check-equal? (rshift "" 10) "")
  (check-equal? (rshift "Black waters" 3) "ers")
  (check-equal? (rshift "Black waters" 0) "")
  (check-equal? (rshift "Black waters" 100) "Black waters")
  (check-equal? (rshift '(1 2 3 4 5 6 7 8 9)) '(9))
  (check-equal? (rshift '(1 2 3 4 5 6 7 8 9) 2) '(8 9))

  (check-equal? (rpop '(1 2 3 4 5 6 7 8 9)) 9)
  (check-equal? (last "Andromachus") "s")

  (check-equal? (rtrim "" 10) "")
  (check-equal? (rtrim "Oslo god morgen" 0) "Oslo god morgen")
  (check-equal? (rtrim "Oslo god morgen" 9) "Oslo g")
  (check-equal? (rtrim "Oslo god morgen" 100) "")
  (check-equal? (rtrim "Oslo god morgen" -5) "") ; add contract!
  (check-equal? (rtrim '(1 2 3 4 5 6 7 8 9) 2) '(1 2 3 4 5 6 7))

  (check-equal? (rpush '() 100) '(100))
  (check-equal? (rpush '(1 2 3) 100) '(1 2 3 100))
  (check-equal? (rpush '(1 2 3) 3) '(1 2 3 3))

  (check-equal? (rpush-unique '(1 2 3) 100) '(1 2 3 100))
  (check-equal? (rpush-unique '(1 2 3) 3) '(1 2 3))

  (check-equal? (slice "" 1 3) "")
  (check-equal? (slice "Oslo god morgen" 3 1) "")
  (check-equal? (slice "Oslo god morgen" 2 7) "slo go")
  (check-equal? (slice "Oslo god morgen" 4) "o god morgen")
  (check-equal? (slice "Oslo god morgen" 4 100) "o god morgen")
  (check-equal? (slice "Oslo god morgen" 4 -3) "o god morg")
  (check-equal? (slice '() 1 3) null)
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 7) '(2 3 4 5 6 7))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 100) '(2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 100) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 3) '(1 2 3))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -1) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -3) '(1 2 3 4 5 6 7))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) -4 -2) '(6 7 8))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 2) '())
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 3) '(3))

  (check-equal? (merge "hello" " " "world" "!") "hello world!")
  (check-equal? (merge '(1 2 3 4) '(100 200) '("a" "b")) '(1 2 3 4 100 200 "a" "b"))

  (check-equal? (merge-unique '(1 2 3 4) '(100 200)) '(1 2 3 4 100 200))
  (check-equal? (merge-unique '(1 2 3 4) '(5 1 8 2 4)) '(1 2 3 4 5 8))
  (check-equal? (merge-unique '(1 2 3 4) '(5 1 8 2 4) '(0 3 5 7)) '(1 2 3 4 5 8 0 7))
  (check-equal? (merge-unique '(1 2 3 4) 5) '(1 2 3 4))

  (check-equal? (push '(1 2 3 4) '(100 200) '("a" "b")) '((1 2 3 4) (100 200) "a" "b"))

  (check-equal? (concat '(1 2 3 4) '(100 200) '("a" "b")) '((1 2 3 4) (100 200) ("a" "b")))

  (check-equal? (splice
    "Tell me, of that ingenious hero" "O muse, " 10)
    "Tell me, O muse, of that ingenious hero")
  (check-equal? (splice '(1 2 3 4 5 6) '(100 200) 3) '(1 2 100 200 3 4 5 6))

  (check-equal? (remove "Agamemnon" 4) "Agaemnon")
  (check-equal? (remove "Agamemnon" 5 8) "Agamn")
  (check-equal? (remove "Agamemnon" 5 #:len 3) "Agamon")
  (check-equal? (remove "Clytemnestra" 4 -1) "Cly")
  (check-equal? (remove "Oslo" 4) "Osl")
  (check-equal? (remove "Agamemnon" -1) "Agamemno")
  (check-equal? (remove '(1 2 3 4 5 6 7) 4) '(1 2 3 5 6 7))
  (check-equal? (remove '(1 2 3 4 5 6 7) 4 5) '(1 2 3 6 7))
  (check-equal? (remove '(1 2 3 4 5 6 7) 4 #:len 3) '(1 2 3 7))

  (check-equal? (exclude '(1 2 3 4 5 6 7) 3) '(1 2 4 5 6 7))
  (check-equal? (exclude '(1 2 "c" 4 5 "c" 7) "c") '(1 2 4 5 "c" 7))
  (check-equal? (exclude "Tell me, O muse, of that ingenious hero" "o") "Tell me, O muse, f that ingenious hero")

  (check-equal? (exclude-all '(1 2 "c" 4 5 "c" 7) "c") '(1 2 4 5 7))
  (check-equal? (exclude-all "Tell me, O muse, of that ingenious hero" "o") "Tell me, O muse, f that ingenius her")

  (check-equal? (insert "" 0 "") "")
  (check-equal? (insert "" 0 "a") "")
  (check-equal? (insert "" 1 "a") "a")
  (check-equal? (insert "Itaka" 1 "_") "_Itaka")
  (check-equal? (insert "Itaka" 3 "h") "Ithaka")
  (check-equal? (insert "Itaka" 6 "s") "Itakas")
  (check-equal? (insert "Itaka" 7 "z") "Itaka")
  (check-equal? (insert "Itaka" -1 "Y") "ItakaY")
  (check-equal? (insert '(1 2 3 4 5) 3 100) '(1 2 100 3 4 5))

  (check-equal? (setn "" 0 "a") "")
  (check-equal? (setn "" -1 "a") "")
  (check-equal? (setn "" 5 "d") "")
  (check-equal? (setn "Troy" -1 "e") "Troe")
  (check-equal? (setn "Troy" 4 "U") "TroU")
  (check-equal? (setn "Troy" 5 "U") "Troy")
  (check-equal? (setn "Troy" 6 "U") "Troy")
  (check-equal? (setn "Troy" 1 "B") "Broy")
  (check-equal? (setn "Mycenae" -3 "R") "MyceRae")
  (check-equal? (setn "Sparta" 5 "c") "Sparca")
  (check-equal? (setn '(1 2 3 4 5 6 7) 6 100) '(1 2 3 4 5 100 7))

  (check-equal? (setns '(1 2 3 4 5 6 7) '(1 4 6) 100) '(100 2 3 100 5 100 7))
  (check-equal? (setns "Sparta" '(2 3 5) "c") "Sccrca")

  (check-equal? (replace "" "a" "o") "")
  (check-equal? (replace "Homer" "r" " ") "Home ")
  (check-equal? (replace "Sparta" "a" "o") "Sporta")
  (check-equal? (replace "Sparta" "b" "u") "Sparta")
  (check-equal? (replace "Sparta" "S" "This is S") "This is Sparta")
  (check-equal? (replace '(1 2 3 4 5 3) 3 30) '(1 2 30 4 5 3))

  (check-equal? (replace-all "Sparta" "a" "o") "Sporto")
  (check-equal? (replace-all "Tell me, O muse, of that ingenious hero" " " "_") "Tell_me,_O_muse,_of_that_ingenious_hero")
  (check-equal? (replace-all "Tell me, O muse, of that ingenious hero" "w" "_") "Tell me, O muse, of that ingenious hero")

  (check-equal? (not-uniques '(1 2 3 1 10 7 3 4 4)) '(1 3 4))
  (check-equal? (not-uniques "abcdefa") '("a"))
  (check-equal? (not-uniques '(1 2 13 12 10 7 3 4 14)) '())

  (check-equal? (uniques '(1 2 3 1 10 7 3 4 4)) '(1 2 3 10 7 4))

  (check-equal? (minus '() '()) '())
  (check-equal? (minus '(1 2 3) '()) '(1 2 3))
  (check-equal? (minus '() '(1 2 3)) '())
  (check-equal? (minus '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3))

  (check-equal? (intersect '() '()) '())
  (check-equal? (intersect '(1 2 3) '()) '())
  (check-equal? (intersect '() '(1 2 3)) '())
  (check-equal? (intersect '(1 2 3 4 5) '(9 8 7 6 5 4)) '(4 5))

  (check-equal? (difference '() '()) '())
  (check-equal? (difference '(1 2 3) '()) '(1 2 3))
  (check-equal? (difference '() '(1 2 3)) '(1 2 3))
  (check-equal? (difference '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3 9 8 7 6))

  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(3 1 2)) '(9))
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(3 1 2 0)) '9)
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(2)) '(4 5))
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(1 0)) 2)

  (check-equal? (dupstr "a" 5) "aaaaa")
  (check-equal? (dupstr "foo " 3) "foo foo foo ")

  (check-equal? (partition '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
  (check-equal? (partition '(1 2 3 4 5 6 7 8 9) 4) '((1 2 3 4) (5 6 7 8)))
  (check-equal? (partition '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
  (check-equal? (partition '(1 2 3 4 5) 0) '(1 2 3 4 5))

  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 4) '((1 2 3 4) (5 6 7 8) (9)))
  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12) (13 14 15)))
  (check-equal? (partition-all '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
  (check-equal? (partition-all '(1 2 3 4 5) 0) '(1 2 3 4 5))

  (check-equal? (flatten '((1 2 3) (4 5 6))) '(1 2 3 4 5 6))
  (check-equal? (flatten '((1 (2 (3))) (4 ((5)) 6) 7)) '(1 2 3 4 5 6 7))
  (check-equal? (flatten '(1 2 3)) '(1 2 3))

  (check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))

  (check-equal? (map-cycled (λ (a b c) (+ b c)) '(1 2 3 4 5 6) '(10 20) '(5 15 25)) '(15 35 35 25 25 45))

  (check-equal? (soft-merge "c") "c")
  (check-equal? (soft-merge 567) 567)
  (check-equal? (soft-merge 1 2) 3)
  (check-equal? (soft-merge 1 2.5) 3.5)
  (check-equal? (soft-merge 1 2.5 #:op -) -1.5)
  (check-equal? (soft-merge #:op (λ args (/ (apply + args) (len args))) 1 3 4 5 7) 4)
  (check-equal? (soft-merge 1 "a") "1a")
  (check-equal? (soft-merge "bc" 2 48.5 "o") "bc248.5o")
  (check-equal? (soft-merge "c" "de") "cde")
  (check-equal? (soft-merge "moscow" " " "calling") "moscow calling")
  ;(check-equal? (soft-merge '(1 2) 5) '(1 2 5))
  ;(check-equal? (soft-merge "d" (hash 'a 10 'b 20)) "d")
  ;(check-equal? (soft-merge (hash 'a 10 'b 20) "d") (hash 'a 10 'b 20 "d" null))
)
