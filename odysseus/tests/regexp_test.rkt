#lang racket

(require rackunit)
(require "../regexp.rkt")

(define foovar 100)

(check-true (re? #rx"d[a-z]*\\d+$"))
(check-true (re? (regexp "d[a-z]*\\d+$")))
(check-true (re? (pregexp "d[a-z]*\\d+$")))

(check-true (re? (->pre (regexp "abc"))))
(check-true (re? (->pre "abc")))
(check-true (re? (->pre 123)))
(check-false (re? 123))

(check-true (re-matches? "row" "barrowbee"))
(check-true (re-matches? "row|kaa" "barrowbee"))
(check-true (re-matches? "swan|bee" "barrowbee"))
(check-true (re-matches? "mt:|ct:" "ct:gene"))
(check-true (re-matches? "\\d+bar" "3barrowbee"))
(check-true (re-matches? "\\d+barrowbee" "345barrowbee"))
(check-true (re-matches? "m[abuws]{2}e" "Tell me, O muse, of that ingenious hero"))
(check-true (re-matches? (regexp "m[abuws][abuws]e") "Tell me, O muse, of that ingenious hero"))
(check-true (re-matches? (pregexp "m[abuws]{2}e") "Tell me, O muse, of that ingenious hero"))
(check-false (re-matches? "row" "doo"))

(check-false (re-full-matches? "row" "barrowbee"))
(check-false (re-full-matches? "\\d+bar" "3barrowbee"))
(check-true (re-full-matches? "\\d+barrowbee" "345barrowbee"))

(check-equal? (get-matches "a.*b" "barrowbee") '(("arrowb")))
(check-equal? (get-matches (regexp "a.*b") "barrowbee") '(("arrowb")))
(check-equal? (get-matches (pregexp "a.*b") "barrowbee") '(("arrowb")))
(check-equal? (get-matches "a(.*)b" "barrowbee") '(("arrowb" "rrow")))
(check-equal? (get-matches #rx"{(.*)}" "b{arrow}bee") '(("{arrow}" "arrow")))
(check-equal? (get-matches "m(.*)o" "barrowbee") empty)
(check-equal?
  (get-matches #px"node/(\\S+)/(\\S+)" "node/a1468/index.php node/other/do~")
  '(("node/a1468/index.php" "a1468" "index.php")
    ("node/other/do~" "other" "do~")))
(check-equal? (get-matches "City of (Moscow|Athens) is a capital of (Russia|Greece)" "City of Moscow is a capital of Russia")
    '(("City of Moscow is a capital of Russia" "Moscow" "Russia")))
(check-equal? (get-matches "City of (Moscow|Athens) is a capital of (Russia|Greece)" "Tallas")
    empty)

(check-equal? (get-first-group-match "a(.*)b" "barrowbee") "rrow")
(check-equal? (get-first-group-match "a(.*)b" "zoomanoo") #f)

(check-equal? (re-substitute "some (text)" "\\(" "[") "some [text)")
(check-equal? (re-substitute "some (text)" "\\(.*?\\)" "[]") "some []")
(check-equal? (re-substitute "some (text)" (list "o" "e") (list "a" "i")) "sami (tixt)")
(check-equal? (re-substitute "some (text)" (list "s.*(?=\\s)" "e") (list "any" "i")) "any (tixt)")

(check-equal? (re-format "~a ~a,~a" "foo" "bar" "baz") #rx"foo bar,baz")
(check-equal? (re-format "~a ~a,~a" "foo" #rx"bar" "baz") #rx"foo bar,baz")
(check-equal? (re-format "~a ~a,~a" "foo" #rx"bar" #px"baz") #px"foo bar,baz")
