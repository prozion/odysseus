#lang racket

(require racket/cmdline)

(require "../lib/all.rkt")
(require "../scrap/vk/vk.rkt")
(require "../scrap/vk/users.rkt")
(require "../scrap/vk/groups.rkt")
(require "../graphics/console.rkt")
(require "../reports/html.rkt")

(provide (all-defined-out))

(define intersects "")
(define output-file "")

(command-line
  #:program "vk-groups, utility to work with groups in vk.com"
  #:multi
    [("-i" "--intersect") i
                    "groups intersection, write groups ids through a space"
                    (set! intersects (split i " "))]
    [("-o" "--output") o
                    "redirect output to file in the current directory"
                    (set! output-file o)]
    [("-s" "--status")
                    "display the process progress"
                    (status-output #t)]
  #:args
    ()
    (cond
      ((notnil? intersects)
          (if (notnil? output-file)
            (write-html-file
              output-file
              "Пересечение групп"
              (map vk/id->href (apply vk/intersect-groups intersects)))
            (display (format "~nПересечение по участникам: ~a человек~n" (length (apply vk/intersect-groups intersects))))))
      (else
        (newline)
        (display "какой запрос вы хотите выполнить?")
        (void)))
)
