#lang racket

(require net/url)
(require "../lib/all.rkt")
(require json)

(provide (all-defined-out))

(define (get-links-to-articles page)
  (let* ((html (get-url (format "https://en.wikipedia.org/wiki/~a" page)))
        ; cut html at <id="See_also"> to avoid navigation links
        (links-urls (get-matches (pregexp "\"/wiki/([A-Za-z0-9_\\(\\)\\-]+)") html))
        (links (map (curry second) links-urls)))
    (uniques (minus links (list page (format "~a_(disambiguation)" page) "File" "Wikipedia" "Category" "Portal" "Talk" "Main_Page")))))
