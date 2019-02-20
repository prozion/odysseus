#lang racket

(require "../../lib/_all.rkt")

(require sxml)
(require html-parsing)

(provide (all-defined-out))

(define (yr/get-xml-data place region country)
  (html->xexp
    (get-url (format "http://www.yr.no/place/~a/~a/~a/forecast.xml" country region place))))

(define (yr/get-time xexp)
  ((sxpath '(weatherdata forecast tabular time)) xexp))

(define (yr/get-clouds time-xexps (time ""))
  (filter
    (λ (z) (regexp-match (regexp "21:00") (nth z 1)))
    (map (λ (x) (list
                  (time-reformat (nlist-ref ((sxpath '(time @ from)) x) (list 0 1)))
                  (time-reformat (nlist-ref ((sxpath '(time @ to)) x) (list 0 1)))
                  (nlist-ref ((sxpath '(time symbol @ name)) x) (list 0 1))))
          (map
            (λ (y) (lpush (list y) '*TOP*))
            time-xexps))))
