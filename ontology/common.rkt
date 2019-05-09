#lang racket

(require "../lib/_all.rkt")
(require racket/syntax)

(provide (all-defined-out))

(define general-namespaces
  (hash
    'owl "http://www.w3.org/2002/07/owl#"
    'rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    'rdfs "http://www.w3.org/2000/01/rdf-schema#"
    'skos "http://www.w3.org/2004/02/skos/core#"
    'dcterms "http://purl.org/dc/terms"
    'dcels "http://purl.org/dc/elements/1.1"
    'dcam "http://purl.org/dc/dcam"))

(define general-namespaces-reverted (hash-revert general-namespaces))

(define keys-order '(a subclass-of subproperty-of eq-property domain range inverse-of disjoint-with description d comment note))

(define-catch (get-namespace val)
  (let ((val (->string val)))
    (cond
      ((re-matches? "#" val)
        (format-symbol "~a#" (first (string-split val "#"))))
      (else
        (format-symbol "~a" (implode (but-last (string-split val "/")) "/"))))))

(define-catch (get-pure-name val)
  (let ((val (->string val)))
    (cond
      ((re-matches? "#" val)
        (->symbol (second (string-split val "#"))))
      (else
        (->symbol (last (string-split val "/")) "/")))))

(define-catch (replace-namespace-for-alias val)
  (let* ((namespace (get-namespace val))
        (name (get-pure-name val))
        (alias (hash-ref* general-namespaces-reverted namespace #f))
        (result (if alias
                    (format-symbol "~a/~a" alias name)
                    (format-symbol "~a" name))))
    result))

(define-catch (get-namespaces-in-use context)
  (opt/uniques
    (flatten
      (map
        (λ (x)
          (map get-namespace (hash-values x)))
        context))))
