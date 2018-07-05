#lang racket

(require "../release.rkt")

(extract-files
  (list
    "lib")
  ""
  #:extract-to "c:/denis/denis_core/projects/sbgn-pd2af/server"
  #:exception-set (list
    "lib/projects/gis"
    "lib/tests"
))
