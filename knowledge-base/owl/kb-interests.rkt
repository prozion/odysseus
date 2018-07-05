#lang racket

(require "../../lib/load/all.rkt")
(require "../owl/turtle.rkt")
(require "../tab-tree.rkt")

(provide (all-defined-out))

;;; Build ontology for hobbies and professions

(define hobbies-tree (parse-tab-tree "c:/denis/denis_core/denis_ontologies/interests/hobbies.tree"))

(define professions-tree (parse-tab-tree "c:/denis/denis_core/denis_ontologies/interests/professions.tree"))

; (define skills-tree (parse-tab-tree "c:/denis/denis_core/denis_ontologies/interests/skills.tree"))

(subclass- Хобби hobbies-tree)
(subclass- Профессия professions-tree)
