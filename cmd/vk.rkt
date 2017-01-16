#lang racket

(require racket/cmdline)

(require "../lib/all.rkt")
(require "../scrap/vk/all.rkt")
(require "../graphics/console.rkt")
(require (only-in "globals.rkt" friends-limit status-output)) ; STX only-in as all-defined-out

(provide (all-defined-out))

(define query "")
(define users "")
(define output-file "")

(command-line
  #:program "vk"
  #:multi
    [("-u" "--users") u
                    "two users to find path from one to another, write only ids, space-divided, e.g. -u \"1 5\""
                    (set! users u)]
    [("-o" "--output") o
                    "redirect output to file in the current directory"
                    (set! output-file o)]
    [("-f" "--friends-limit") fl
                    "limit for requested friends per each user to make search faster, but less precise"
                    (friends-limit (string->number fl))]
    [("-s" "--status")
                    "display the search process progress"
                    (status-output #t)]
  #:args
    ()
    (let* ( (us (split users " "))
            (u1 (nth us 1))
            (u2 (nth us 2)))
      (cond
        ((notnil? output-file)
            (vk/alist->html
              output-file
              "Цепочка связей"
              (vk/ids->hrefs (vk/find-paths u1 u2))))
        (else
          (newline)
          (displayln (vk/find-paths u1 u2))
          (void))))
)
