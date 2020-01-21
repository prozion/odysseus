#lang racket

(require "base.rkt")
(require "seqs.rkt" (for-syntax "seqs.rkt"))
(require compatibility/defmacro)

(provide (all-defined-out))

(define (re? x)
  (or (regexp? x) (pregexp? x)))

(define (->pre x)
  (cond
    ((re? x) x)
    ((string? x) (pregexp x))
    (else (pregexp (str x)))))

(define (->re x)
  (cond
    ((re? x) x)
    ((string? x) (regexp x))
    (else (regexp (str x)))))

(define (re-matches? re astr)
  (true? (regexp-match (->pre re) astr)))

(define (re-full-matches? re astr)
  (true? (re-matches? (str "^" re "$") astr)))

(define-catch (get-matches re astr)
  (let ((re (->re re)))
    (for/fold
      ((res (list)))
      ((match-position (regexp-match-positions* re astr)))
      ; BUG: gives an error, when parsing timeline.tree
      (begin
        ; (println (format "~a ~a ~a ~a" re astr match-position (regexp-match-positions* re astr)))
        (pushr
          res
          (regexp-match
            re
            astr
            (car match-position)))))) )

(define (get-first-group-match re astr)
  (let* ((res (get-matches re astr)))
    (match res
      (`((,_ ,g1)) g1)
      (else #f))))

(define (re-substitute astr re repstr)
  (cond
    ((and (list? re) (list? repstr))
      (if (or (null? (cdr re)) (null? (cdr repstr)))
        (re-substitute astr (car re) (car repstr))
        (re-substitute (re-substitute astr (car re) (car repstr)) (cdr re) (cdr repstr))))
    (else
      (string-replace astr (->pre re) repstr))))

(define (regular-expression-string? astr)
  (or
    (string-contains? astr "+?")
    (string-contains? astr "*?")
    (string-contains? astr "]*")
    (string-contains? astr "]+")))

(module+ test

  (require rackunit)

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
)
