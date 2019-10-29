#lang racket

(require "../../lib/_all.rkt")
(require "../tab-tree.rkt")

(provide (all-defined-out))

(define (write-kb iri . filenames)
  (write-file
    (str (ontology-name iri) ".ttl")
    (str
      (header iri)
      (tab-tree->owl
        (for/fold
          ((res (list)))
          ((f filenames))
          (let ((filename (if (cons? f) (car f) f))
                (rules (if (cons? f) (cdr f) #f)))
            (append
              res
              (if rules
                (parse-tab-tree filename #:ontology? #t #:rules rules)
                (parse-tab-tree filename #:ontology? #t)))))))))
