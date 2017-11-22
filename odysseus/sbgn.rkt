#lang racket

(require "release.rkt")

(extract-files
  (list
    "lib"
    "sbgn")
  "odysseus-sbgn"
  #:exception-set (list
    "sbgn/atoms.rkt"
    "sbgn/molecules.rkt"
    "lib/tests"
  )
)
