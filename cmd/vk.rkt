#lang racket

(require racket/cmdline)

(require "../lib/all.rkt")
(require "../scrap/vk/all.rkt")
(require "../graphics/console.rkt")

(define query "")
(define users "")
(define output-file "")

(command-line
  #:program "vk"
  #:multi
    [("-u" "--users") u
                    "two users to find path from one to another"
                    (set! users u)]
    [("-o" "--output") o
                    "redirect output to file"
                    (set! output-file o)]
  #:args
    ()
    (let* ( (us (split users " "))
            (u1 (nth us 1))
            (u2 (nth us 2)))
      (cond
        ((notnil? output-file)
          (vk-alist->html
            output-file
            "Цепочка связей"
            (ids->hrefs (find-paths u1 u2))))
        (else
          (newline)
          (displayln (find-paths u1 u2))
          (void))))
)
