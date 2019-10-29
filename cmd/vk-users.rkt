#lang racket

(require racket/cmdline)

(require "../lib/_all.rkt")
(require "../scrap/vk.rkt")
(require "../scrap/vk-friends.rkt")
(require "../graphics/console.rkt")
(require "../report/html.rkt")

(provide (all-defined-out))

(define query "")
(define users "")
(define output-file "")

(command-line
  #:program "vk-users, utility to work with users data in vk.com"
  #:multi
    [("-c" "--chain") u
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
            (write-html-file
              output-file
              "Цепочка связей"
              (vk/ids->hrefs (vk/find-paths u1 u2))))
        (else
          (newline)
          (displayln (vk/find-paths u1 u2))
          (void))))
)
