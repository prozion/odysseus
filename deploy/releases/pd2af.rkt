#lang racket

(require "../release.rkt")

(extract-files
  (list
    "lib"
    "knowledge-base"
    "graphics")
  #:new-directory "odysseus"
  #:extract-to "c:/denis/denis_core/projects/pd2af/libs/"
  #:exception-set (list
    "lib/tests"
    "knowledge-base/neo4j"
    "knowledge-base/owl"
    "graphics/tests"
))
