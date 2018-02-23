#lang racket

(require "../release.rkt")

(extract-files
  (list
    "lib"
    "sbgn")
  "sbgn-simulation"
  #:exception-set (list
    "sbgn/chemistry"
    "sbgn/pd"
    "sbgn/af"
    "lib/tests"
))
