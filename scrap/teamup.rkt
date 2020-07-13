#lang racket

(require "../lib/_all.rkt")
(require "../../settings/APIs.rkt")
(require net/url)
(require net/head)
(require json)
(require compatibility/defmacro)

(provide (all-defined-out))

(define-catch (get-events calendar-id api-key #:start-date (start-date #f) #:end-date (end-date #f))
  (let* (
        (start-date (and start-date (dd.mm.yyyy->yyyy-mm-dd start-date)))
        (end-date (and end-date (dd.mm.yyyy->yyyy-mm-dd end-date)))
        (header (list (format "Teamup-Token: ~a" api-key)))
        (req (format "https://api.teamup.com/~a/events?~a~a"
                      calendar-id
                      (if start-date
                          (format "startDate=~a" start-date)
                          "")
                      (cond
                          ((and start-date end-date)
                              (format "&endDate=~a" end-date))
                          (end-date
                              (format "endDate=~a" end-date))
                          (else ""))
                      ))
        (res (get-url req #:header header))
        (res (string->jsexpr res))
        )
    ($ events res)))

(define example
  #hasheq(
    (all_day . #f)
    (creation_dt . 2018-06-29T11:07:01+03:00)
    (delete_dt . null)
    (end_dt . 2018-07-21T13:00:00+03:00)
    (id . 196379514)
    (location . "")
    (notes . null)
    (readonly . #f)
    (remote_id . 7D25A62A-59A3-420A-A2E4-EBAD72963509)
    (ristart_dt . null)
    (rrule . "")
    (rsstart_dt . null)
    (series_id . null)
    (start_dt . 2018-07-21T11:00:00+03:00)
    (subcalendar_id . 1267626)
    (subcalendar_ids . (1267626))
    (title . Силкин)
    (tz . null)
    (update_dt . null)
    (version . 5b35e8a5d7c6d)
    (who . "")))
