#lang racket

(require "../lib/_all.rkt")

(provide (all-defined-out))

(define (pth ref)
  (format  "../../~a" ref))

(define (leaf->filename leaf-path)
  (str (implode leaf-path "/") ".tree"))

(define (ref-triplets refs-hash-tree)
  (let* ((paths (get-paths refs-hash-tree)))
    (for/fold
        ((res empty))
        ((p paths))
        (let* ((path (rtrim p 2))
              (base-leaf (last (but-last p)))
              (referred-leaf-id (last p))
              (referred-leaf (hash-tree-get-value-by-id-path refs-hash-tree p))
              (refs ($ ref referred-leaf))
              (refs (split refs ",")))
          (pushr
            res
            (list
              (pth (str (implode path "/") "/" base-leaf ".tree"))
              referred-leaf-id
              (map (λ (x) (pth (str x ".tree"))) refs)
              ($ check-msg referred-leaf)))))))
