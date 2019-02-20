#lang racket

(require net/url)
(require "../lib/_all.rkt")
(require json)

(provide (all-defined-out))

; take only links that are met in the text (avoid plates and navigation), that is try to take context links as generally stronger related to the subject
(define (get-links-to-articles page)
  (let* ((html (get-url (format "https://en.wikipedia.org/wiki/~a" page)))
        (html (car (regexp-split (pregexp "id=\"See_also\"") html))) ; take text before 'See also' section to exclude tons of links after it
        (links-urls (get-matches (pregexp "\"/wiki/([A-Za-z0-9_\\(\\)\\-]+)") html))
        (links (map (curry second) links-urls)))
    (display ".")
    (flush-output)
    (map
      (λ (x) (string-replace
                (string-replace x "(" "\\(")
                ")" "\\)"))
      (uniques
        (minus
          links
          (merge
            (explode "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ; index or something
            (list
              page
              (format "~a_(disambiguation)" page) "File" "Wikipedia" "Category" "Portal" "Talk" "Help" "Main_Page")))))))
