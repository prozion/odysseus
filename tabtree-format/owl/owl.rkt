#lang racket

(require "../lib/_all.rkt")

(provide (all-defined-out))

;;;;; here old functions from tab-tree.rkt:

; (define-catch (fill-tree-iter source-lines result-tree hierarchy-path #:rules (rules (hash 'aa 'subclass 'Aa 'instance 'AA 'is 'aA 'is)) #:root-element (root-element 'root))
;   (cond
;     ((empty? source-lines) result-tree)
;     (else
;       (let* ((line (car source-lines))
;             (item (get-item line))
;             (item-name (hash-ref item '_name))
;             (item-name (->symbol item-name))
; 						(item-relations (hash-delete item '_name))
;             (hierarchy-path (if (empty? hierarchy-path) (list item-name) hierarchy-path))
;             (previous-level (dec (length hierarchy-path)))
;             (new-level (count-tabs line))
;             (hierarchy-path (if (> new-level previous-level)
;                           hierarchy-path
;                           (rtrim (rtrim hierarchy-path 1) (- previous-level new-level))))
;             (parent (if (empty? hierarchy-path) root-element (last hierarchy-path)))
;             (relation (if (title-case? item-name)
;                         (if (title-case? parent)
;                           (hash-ref rules 'AA)
;                           (hash-ref rules 'Aa))
;                         (if (title-case? parent)
;                           (hash-ref rules 'aA)
;                           (hash-ref rules 'aa))
;                       ))
;             (triplet (hash 'subject item-name 'predicate relation 'object parent))
; 						(triplet-with-relations (hash-union triplet item-relations))
;             (new-result-tree (add-item-to-tree result-tree triplet-with-relations new-level)))
;         (fill-ontology-tree-iter (cdr source-lines) new-result-tree (pushr hierarchy-path item-name) #:rules rules #:root-element root-element)))))
