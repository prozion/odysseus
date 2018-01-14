#lang racket

(require "release.rkt")

(extract-files
  (list
    "lib"
    "sbgn")
  "pd2af"
  #:extract-to "c:/denis/denis_core/projects/"
  #:exception-set (list
    "sbgn/chemistry"
    "sbgn/er"
    "sbgn/tests/er"
    "lib/tests"
))
