#lang racket

(require "../lib/all.rkt")
(require "turtle.rkt")

(provide (all-defined-out))

(define-catch (count-tabs line (symbol "\t"))
  (local ((define (count-tabs-iter line symbol count)
                    (cond
                      ((empty? line) count)
                      ((equal? (car line) symbol) (count-tabs-iter (cdr line) symbol (inc count)))
                      (else count))))
    (count-tabs-iter
      (cond
        ((string? line) (explode line))
        ((list? line) line)
        (else line))
      symbol
      0)))

(define-catch (get-item line (symbol "\t"))
  (let* ((res-name
              (get-matches (format "^(~a)*(\\S+)" symbol) line))
        (res-string-parameters
              (get-matches (format "(\\S+):(\".+?\")") line))
        (res-parameters
              (get-matches (format "(\\S+):(\\S+)") line))
				(parameters (for/hash ((p res-parameters)) (values (->symbol (list-ref p 1)) (->symbol (list-ref p 2)))))
				(string-parameters (for/hash ((p res-string-parameters)) (values (->symbol (list-ref p 1)) (list-ref p 2))))
        (name (nth (nth res-name 1) 3)))
      (hash-insert (hash-union string-parameters parameters) (cons '_name name))))

(define-catch (add-item-to-tree tree item level)
  (cond
    ((= level 0) (pushr tree item))
    ((not (list? (last tree))) (pushr tree (list item)))
    (else
      (pushr (rtrim tree 1) (add-item-to-tree (last tree) item (dec level))))))

(define-catch (fill-tree-iter source-lines result-tree)
  (cond
    ((empty? source-lines) result-tree)
    (else
      (let* ((line (car source-lines))
            (level (count-tabs line))
            (item (get-item line))
            (item (->symbol item))
            (new-result-tree (add-item-to-tree result-tree item level)))
        (fill-tree-iter (cdr source-lines) new-result-tree)))))

(define-catch (fill-ontology-tree-iter source-lines result-tree hierarchy-path #:rules (rules (hash 'aa 'subclass 'Aa 'instance 'AA 'is 'aA 'is)) #:root-element (root-element 'root))
  (cond
    ((empty? source-lines) result-tree)
    (else
      (let* ((line (car source-lines))
            (item (get-item line))
            (item-name (hash-ref item '_name))
            (item-name (->symbol item-name))
						(item-relations (hash-delete item '_name))
            (hierarchy-path (if (empty? hierarchy-path) (list item-name) hierarchy-path))
            (previous-level (dec (length hierarchy-path)))
            (new-level (count-tabs line))
            (hierarchy-path (if (> new-level previous-level)
                          hierarchy-path
                          (rtrim (rtrim hierarchy-path 1) (- previous-level new-level))))
            (parent (if (empty? hierarchy-path) root-element (last hierarchy-path)))
            (relation (if (title-case? item-name)
                        (if (title-case? parent)
                          (hash-ref rules 'AA)
                          (hash-ref rules 'Aa))
                        (if (title-case? parent)
                          (hash-ref rules 'aA)
                          (hash-ref rules 'aa))
                      ))
            (triplet (hash 'subject item-name 'predicate relation 'object parent))
						(triplet-with-relations (hash-union triplet item-relations))
            (new-result-tree (add-item-to-tree result-tree triplet-with-relations new-level)))
        (fill-ontology-tree-iter (cdr source-lines) new-result-tree (pushr hierarchy-path item-name) #:rules rules #:root-element root-element)))))

(define-catch (parse-tab-tree tree-file #:ontology? (ontology? #f) #:rules (rules #f))
  (let* (
        (tree-lines (read-file-by-lines tree-file))
        (tree-lines (clean
                      (λ (line) (or
                                  (re-matches? "^\t*;" line)
                                  (re-matches? "^\\s*$" line)))
                      tree-lines)))
    (if ontology?
      (if rules
        (fill-ontology-tree-iter tree-lines (list) (list) #:rules rules)
        (fill-ontology-tree-iter tree-lines (list) (list)))
      (fill-tree-iter tree-lines (list)))))

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
