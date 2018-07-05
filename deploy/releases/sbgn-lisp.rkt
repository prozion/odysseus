#lang racket

(require "../release.rkt")

(extract-files
  (list "lib")
  ""
  #:extract-to "c:/denis/denis_core/projects/sbgn-lisp"
  #:exception-set (list
    "lib/projects"
    "lib/tests"
))
