#lang racket

(require "../lib/all.rkt")
(require "turtle.rkt")

(provide (all-defined-out))

(define ontologies/basic-definitions
  (str
    (ObjectProperty description owl:Thing rdf:langString)
))
