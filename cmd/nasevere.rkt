#lang racket

(require racket/cmdline)

(require "../lib/_all.rkt")
(require "../graphics/console.rkt")
(require "../report/html.rkt")
(require "../report/csv.rkt")
(require "../pis/nasevere.rkt")

(require "../../denis_personal/my_knowledge/nasevere/groups.rkt")

(provide (all-defined-out))

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
  #:program "generate report from the knowledge database"
  #:multi
    [("-o" "--output") o
                    "csv file to output information"
                    (set! output-file o)]

  #:args
    ()
    (cond
      ((notnil? output-file)
        (write-csv-file
          (list "URL группы или предпринимателя" "Название" "Подписчики" "Где размещались" "Кто обслуживает")
          translated-groups
          output-file
          ";"))
      (else
        (display
          (format "~n~a~n" groups))))
)
