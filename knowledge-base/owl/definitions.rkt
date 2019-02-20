#lang racket

(require "../lib/_all.rkt")
(require "turtle.rkt")

(provide (all-defined-out))

(define ontologies/basic-definitions
  (str
    (ObjectProperty 'description owl:Thing rdf:langString)
))

(define people-keys '(name phone city tags surname bdate skype email grave died ddb deathcause bcity sn mbti bank midname address nick old-phone facebook blog icq prof yahoo website profession education languages hobby prof skill))

(define ontologies/people-definitions
  (for/fold
    ((res ""))
    ((key people-keys))
    (str
      res
      (ObjectProperty key Person rdf:langString))))
