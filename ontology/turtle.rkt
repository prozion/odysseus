#lang racket

(require "../lib/all.rkt")
(require racket/stxparam)
(require "../lib/extra/crypto.rkt")

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
  (get-sha (implode (hash-values p))))

;;; rdf description functions ;;;

(define (subClassOf subclass class)
  (format "~a rdfs:subClassOf ~a .~n" (expand-name subclass) (expand-name class)))

(define-syntax-rule (instanceOf instance class)
  (format "~a rdf:type ~a .~n" (expand-name 'instance)  (expand-name 'class)))

(define-syntax-rule (a instance class)
  (format "~a rdf:a ~a .~n" (expand-name 'instance) (expand-name 'class)))

(define-syntax-rule (ObjectProperty name domain-class range-class)
  (str
    (format ":~a rdf:type owl:ObjectProperty .~n" 'name)
    (format ":~a rdfs:domain ~a .~n" 'name (expand-name 'domain-class))
    (format ":~a rdfs:range ~a .~n" 'name (expand-name 'range-class))
  ))

(define (=>> source-object property-name target-object)
  (let ((frmt (if (symbol? target-object) "~a ~a ~a .~n" "~a ~a \"~a\" .~n")))
      (format frmt (expand-name source-object) (expand-name property-name) (expand-name target-object))))
(define-syntax-rule (=> source-object property-name target-object)
  (format "~a ~a ~a .~n" (expand-name 'source-object) (expand-name 'property-name) (expand-name 'target-object)))

(define-syntax-rule (==> source-object property-name target-object)
  (format "~a ~a \"~a\" .~n" (expand-name 'source-object) (expand-name 'property-name) target-object))

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
  (define root (process-iter "" 'root 'lst)))

; fillers
(define (hash->turtle node-name hash-properties)
  (for/fold
    ((res ""))
    ((key (hash-keys hash-properties)))
    (str
      res
      (=>> node-name key (normalize-value (hash-ref hash-properties key))))))

; rootfunc
(define-syntax-rule (rdf iri body ...)
  (write-file
    (str (ontology-name iri) ".owl")
    (str (header iri) body ...)))
