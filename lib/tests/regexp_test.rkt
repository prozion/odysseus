#lang racket

(module+ test

  (require rackunit)
  (require "../regexp.rkt")

  (check-equal? (get-matches "a.*b" "barrowbee") '(("arrowb")))
  (check-equal? (get-matches (regexp "a.*b") "barrowbee") '(("arrowb")))
  (check-equal? (get-matches (pregexp "a.*b") "barrowbee") '(("arrowb")))    
  (check-equal? (get-matches "a(.*)b" "barrowbee") '(("arrowb" "rrow")))
  (check-equal?
    (get-matches "node/(\\S+)/(\\S+)" "node/a1468/index.php node/other/do~")
    '(("node/a1468/index.php" "a1468" "index.php")
      ("node/other/do~" "other" "do~")))
)
