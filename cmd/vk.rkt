#lang racket

(require racket/cmdline)

; stub for showing available vk-utils commands

(command-line
  #:program "vk-users, utility to work with users data in vk.com"
  #:multi
    [("-c" "--chain") u
                    "vk-users: two users to find path from one to another, write only ids, space-divided, e.g. -u \"1 5\""
                    (void)]
    [("-o" "--output") o
                    "vk-users: redirect output to file in the current directory"
                    (void)]
    [("-f" "--friends-limit") fl
                    "vk-users: limit for requested friends per each user to make search faster, but less precise"
                    (void)]
    [("-s" "--status")
                    "vk-users: display the search process progress"
                    (void)]
    [("-i" "--intersect") i
                    "vk-groups: groups intersection, write group ids through a space"
                    (void)]
    [("-o" "--output") o
                    "vk-groups: redirect output to file in the current directory"
                    (void)]
    [("-s" "--status")
                    "vk-groups: display the process progress"
                    (void)]
  #:args
    ()
    (void)
)
