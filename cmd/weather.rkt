#lang racket

(require racket/cmdline)

(require "../lib/load/all.rkt")
(require "../graphics/console.rkt")
(require "../reports/html.rkt")

(require "../scrap/weather/weather.rkt")
(require "../scrap/weather/yr.no.rkt")

(provide (all-defined-out))

(define place "Nikel")
(define region #f)
(define country #f)

(command-line
  #:program "weather from yr.no"
  #:multi
    [("-p" "--place") p
                    "place for weather forecast"
                    (set! place p)]
    [("-r" "--region") r
                    "region of the place"
                    (set! region r)]
    [("-c" "--country") c
                    "country"
                    (set! country c)]
  #:args
    ()
    (display (format "~n~a~n" (alist->mstring
                                (yr/get-clouds
                                  (yr/get-time
                                    (yr/get-xml-data
                                      place
                                      (if region region "Murmansk")
                                      (if country country "Russia"))))
                                "~a - ~a    ~a"
                           )))
)
