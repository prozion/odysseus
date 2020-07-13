#lang racket

(require "../../lib/_all.rkt")
(require racket/stxparam)
(require "../../lib/crypto.rkt")

(provide (all-defined-out))

(define (turtle filename)
  (read-file filename))

(define (header ontology-iri)
  (str
    (format "@prefix : <~a#> .\n" ontology-iri)
    "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n"
    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
    "@prefix xml: <http://www.w3.org/XML/1998/namespace> .\n"
    "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n"
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n"
    (format "@base <~a> .~n~n" ontology-iri)
    (format "<~a> rdf:type owl:Ontology .~n~n" ontology-iri)))

;;; helpers ;;;

(define (ontology-name iri)
  (last (split iri "/")))

(define (local-namespace? var)
  (not (string-contains? var ":")))

(define (expand-name var)
  (cond
    ((symbol? var)
      (let ((var (symbol->string var)))
        (string->symbol (if (local-namespace? var) (str ":" var) var))))
    (else var)))

(define (normalize-value val)
  (let ((newval (if (list? val) (implode val " ") val)))
    newval))

(define (sha-name p)
  (str "a" (get-sha (implode (hash-values p)))))

(define (short-sha-name p)
  (substring (sha-name p) 0 12))

;;; rdf description functions ;;;

(define (subClassOf subclass class)
  (format "~a rdfs:subClassOf ~a .~n" (expand-name subclass) (expand-name class)))

(define (instanceOf instance class)
  (format "~a rdf:type ~a .~n" (expand-name instance)  (expand-name class)))

; (define-syntax-rule (instanceOf instance class)
;   (format "~a rdf:type ~a .~n" (expand-name 'instance)  (expand-name 'class)))

(define-syntax-rule (a instance class)
  (format "~a rdf:a ~a .~n" (expand-name 'instance) (expand-name 'class)))

(define-syntax-rule (ObjectProperty name domain-class range-class)
  (str
    (format ":~a rdf:type owl:ObjectProperty ;~n" name)
    (format "rdfs:domain ~a ;~n" (expand-name 'domain-class))
    (format "rdfs:range ~a .~n" (expand-name 'range-class))
))

;;

(define (n-n-n source-object property-name target-object)
  (let ((frmt (if (symbol? target-object) "~a ~a ~a .~n" "~a ~a \"~a\" .~n")))
      (format frmt (expand-name source-object) (expand-name property-name) (expand-name target-object))))

(define (n-n-n- source-object property-name target-object)
  (let ((frmt (if (symbol? target-object) "~a ~a ~a ;~n" "~a ~a \"~a\" ;~n")))
      (format frmt (expand-name source-object) (expand-name property-name) (expand-name target-object))))

(define (n-n property-name target-object)
  (let ((frmt (if (symbol? target-object) "~a ~a ;~n" "~a \"~a\" ;~n")))
      (format frmt (expand-name property-name) (expand-name target-object))))

(define (n-n. property-name target-object)
  (let ((frmt (if (symbol? target-object) "~a ~a .~n" "~a \"~a\" .~n")))
      (format frmt (expand-name property-name) (expand-name target-object))))

(define-syntax-rule (N-N-N source-object property-name target-object)
  (format "~a ~a ~a .~n" (expand-name 'source-object) (expand-name 'property-name) (expand-name 'target-object)))

(define-syntax-rule (N-N property-name target-object)
  (format "~a ~a ;~n" (expand-name 'property-name) (expand-name 'target-object)))

(define-syntax-rule (N-N. property-name target-object)
  (format "~a ~a .~n" (expand-name 'property-name) (expand-name 'target-object)))

(define-syntax-rule (N-N-n source-object property-name target-object)
  (format "~a ~a \"~a\" .~n" (expand-name 'source-object) (expand-name 'property-name) target-object))

(define-syntax-rule (N-n property-name target-object)
  (format "~a \"~a\" ;~n" (expand-name 'property-name) target-object))

(define-syntax-rule (N-n. property-name target-object)
  (format "~a \"~a\" .~n" (expand-name 'property-name) target-object))

; (define-syntax (===> stx)
;   (syntax-case stx ()
;     ((_ source-object property-name target-object)
;       #'(format "~a ~a ~a .~n" (expand-name 'source-object) (expand-name 'property-name) (expand-name 'target-object)))

(define-syntax-rule (sameAs class1 class2)
  (format "~a owl:sameAs ~a .~n" (expand-name 'class1) (expand-name 'class2)))

(define (process-iter acc root lst)
  (cond
    ((empty? lst) acc)
    ((= (length lst) 1) (format "~a~a" acc (subClassOf (car lst) root)))
    ((not (list? (second lst)))
      (process-iter
         (format "~a~a" acc (subClassOf (car lst) root))
         root
         (cdr lst)))
    ((list? (second lst))
      (process-iter
        (process-iter
           (format "~a~a" acc (subClassOf (car lst) root))
           (first lst)
           (second lst))
        root
        (cddr lst)))
    (else acc)))

(define-syntax-rule (subclass root lst)
  (define root
    (str
      (subClassOf 'root 'owl:Thing)
      (process-iter "" 'root 'lst))))

(define-syntax-rule (subclass- root lst)
  (define root
    (str
      (subClassOf 'root 'owl:Thing)
      (process-iter "" 'root lst))))

; fillers
(define (valid-keys keys)
  (filter
    (λ (x) (empty? (intersect (explode (symbol->string x)) '("?"))))
    keys))

(define (hash->turtle hash-properties #:breakable-fields (breakable-fields #f))
  (let* ((keys (valid-keys (hash-keys hash-properties)))
        (lastkey (last keys)))
    (for/fold
      ((res ""))
      ((key keys))
      (let* ((value (hash-ref hash-properties key))
            (value (if (and
                          breakable-fields
                          (indexof? breakable-fields key)
                          (string? value))
                      (split value " ") (list value))))
        (for/fold
          ((s res))
          ((v value))
          (str
            s
            (if (and (equal? key lastkey) (equal? v (last value)))
              (n-n. key (normalize-value v))
              (n-n key (normalize-value v)))))))))

; rootfunc
(define-syntax-rule (rdf iri body ...)
  (write-file
    (str (ontology-name iri) ".ttl") ; .ttl instead of .owl or .rtf as Fuseki wants .ttl extension while uploading
    (str (header iri) body ...)))

(define (owl-expression subject predicate object)
  (cond
    ((equal? predicate 'subclass) (subClassOf subject object))
    ((equal? predicate 'instance) (instanceOf subject object))
    (else (format "~a ~a ~a .~n" (expand-name subject) (expand-name predicate) (expand-name object)))))

(define (tab-tree->owl tab-tree)
  (local ((define (item->string item-hash)
            (let* ((subject (hash-ref tab-tree 'subject))
                  (predicate (hash-ref tab-tree 'predicate))
                  (object (hash-ref tab-tree 'object))
                  (properties (hash-delete-all tab-tree '(subject predicate object))))
              (for/fold
                ((res (owl-expression subject predicate object)))
                ((k (hash-keys properties)))
                (str
                  res
                  (let* ((vs (hash-ref properties k))
                        (vs (cond
                              ((symbol? vs) (map string->symbol (split (symbol->string vs) ",")))
                              ((string? vs) (split vs ","))
                              (else vs))))
                    (for/fold
                      ((res2 ""))
                      ((v vs))
                        (str
                          res2
                          (format "~a ~a ~a .~n" (expand-name subject) (expand-name k) (expand-name v))))))))))
    (cond
      ((hash? tab-tree) (item->string tab-tree))
      ((empty? tab-tree) "")
      ((list? tab-tree) (string-append (tab-tree->owl (car tab-tree)) (tab-tree->owl (cdr tab-tree))))
      (else ""))))
