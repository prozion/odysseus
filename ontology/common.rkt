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
    'dcterms "http://purl.org/dc/terms/"
    'dcels "http://purl.org/dc/elements/1.1/"
    'dcam "http://purl.org/dc/dcam/"
    'vocab "http://www.w3.org/2003/06/sw-vocab-status/ns#"))

(define general-namespaces-reverted (hash-revert general-namespaces))

(define keys-order '(a instance-of subclass-of subproperty-of eq-property domain range inverse-of disjoint-with title description d comment note publisher modified))

(define own-namespace (make-parameter #f))

(define-catch (get-namespace val)
  (let ((valstr (->string val)))
    (cond
      ((not val) #f)
      ((empty? val) #f)
      ((list? val)
        (map get-namespace val))
      ((not (re-matches? "/" valstr)) #f)
      ((re-matches? " " valstr) #f)
      ((re-matches? "#" valstr)
        (format-symbol "~a#" (first (string-split valstr "#"))))
      (else
        (format-symbol "~a/" (implode (but-last (string-split valstr "/")) "/"))))))

(define-catch (get-pure-name val)
  (let ((valstr (->string val)))
    (cond
      ((not val) #f)
      ((empty? val) #f)
      ((re-matches? "#" valstr)
        (->symbol (second (string-split valstr "#"))))
      (else
        (->symbol (last (string-split valstr "/")) "/")))))

(define-catch (replace-namespace-for-alias val)
  (let* ((namespace (get-namespace val))
        (own-ns (own-namespace))
        (own-ns (and own-ns (hash-ref general-namespaces own-ns #f)))
        (own-ns (and own-ns (->symbol own-ns)))
        (name (get-pure-name val))
        (alias (and namespace (hash-ref* general-namespaces-reverted namespace #f)))
        (result (cond
                  ((empty? val) #f)
                  ((not (or name alias)) #f)
                  ((and own-ns (equal? namespace own-ns))
                    (--- own-ns namespace)
                    (format-symbol "~a" name))
                  ((and name (not alias))
                    (format-symbol "~a" name))
                  (else
                    (format-symbol "~a/~a" alias name)))))
    result))

(define-catch (get-namespaces-in-use context)
  (cleanmap
    (opt/uniques
      (flatten
        (map
          (λ (x)
            (map get-namespace (append (hash-keys x) (hash-values x))))
          context)))))
