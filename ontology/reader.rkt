#lang racket

(require "../lib/_all.rkt")
(require "../tabtree-format/tab-tree.rkt")

(provide (all-defined-out))

(define-catch (read-ontology filename)
  (let* ((extension (last (string-split filename ".")))
        (context (cond
                    ((equal? extension "mtree") (parse-tab-mtree filename))
                    ((equal? extension "tree") (parse-tab-tree filename))
                    (else (error (format "wrong ontology file extension: ~a" extension)))))
        )
    (--- context)
    context))
