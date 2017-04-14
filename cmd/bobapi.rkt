#lang racket

(require racket/cmdline)

(require "../lib/all.rkt")
(require "../graphics/console.rkt")
(require "../reports/html.rkt")
(require "../reports/csv.rkt")
(require "../scrap/bobapi.rkt")

(require "../../settings/APIs.rkt")

(provide (all-defined-out))

(define temperature-query "")
(define output-file "")

(define translated-groups
  (map
    (λ (x)  (list
              (nth x 1)
              (nth x 2)
              (nth x 3)
              (tags->publish-status (nth x 4))
              (tags->owner (nth x 4))))
    (partition groups 4)))

(command-line
  #:program "generate reports from the knowledge database"
  #:multi
    [("-T" "--temperature") t
                    "body temperature data"
                    (set! temperature-query t)]
    [("-o" "--output") o
                    "csv file to output information"
                    (set! output-file o)]

  #:args
    ()
    (cond
      ((notnil? output-file)
        (write-csv-file
          (list "header1" "header2")
          (list (list "data1" "data2"))
          output-file
          ","))
      (else
        (display (get-current-access-token bob-api-access-token)))
)
