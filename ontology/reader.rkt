#lang racket

(require sxml)
(require racket/syntax)
(require "../lib/_all.rkt")
(require "../tabtree-format/tab-tree.rkt")
(require "common.rkt")

(provide (all-defined-out))

(define-catch (read-tabtree-ontology path)
  (let* ((extension (get-file-extension path))
        (context (cond
                    ((equal? extension "mtree") (parse-tab-mtree path))
                    ((equal? extension "tree") (parse-tab-tree path))
                    (else (error (format "wrong ontology file extension: ~a" extension)))))
        )
    context))

(define count (make-parameter 0))

(define-catch (build-item item-sxml (ontology-id 'root))
  (match item-sxml
    ; meta information
    (`(,_ ,_
        (dcterms:contributor ,contributor) ...
        (dcterms:creator ,creator) ...
        (dcterms:description (@ (xml:lang ,_)) ,description) ...
        (dcterms:modified ,modified) ...
        (dcterms:publisher (@ (xml:lang ,_)) ,publisher) ...
        (dcterms:title (@ (xml:lang ,_)) ,title)
        (rdfs:comment ,comment) ...
        (rdfs:seeAlso (@ (rdf:resource ,see-also))) ...
        )
          (hash
                'id (->symbol ontology-id)
                '_parent #f
                'a "owl/Ontology" 'title title 'publisher publisher 'comment comment 'modified modified))
    ; class
    (`(rdf:Description (@ (rdf:about ,about))
        (dcterms:description (@ (xml:lang ,_)) ,descriptions) ...
        (dcterms:hasVersion (@ (rdf:resource ,has-versions))) ...
        (dcterms:issued ,issueds) ...
        (dcterms:modified ,modifieds) ...
        (owl:disjointWith (@ (rdf:resource ,disjoint-withs))) ...
        (rdf:type (@ (rdf:resource ,types))) ...
        (rdfs:comment (@ (xml:lang ,_)) ,comments) ...
        (rdfs:isDefinedBy (@ (rdf:resource ,is-defined-bys))) ...
        (rdfs:label (@ (xml:lang ,lang)) ,labels) ...
        (rdfs:seeAlso (@ (rdf:resource ,see-alsoes))) ...
        (rdfs:subClassOf (@ (rdf:resource ,subclass-ofs))) ...
        (skos:definition (@ (xml:lang ,_)) ,definitions) ...
        (skos:example (@ (xml:lang ,_)) ... ,examples) ...
        (skos:note (@ (xml:lang ,_)) ,notes) ...
        (skos:scopeNote (@ (xml:lang ,_)) ,scope-notes) ...
        )
          (let* ((about (get-pure-name about)))
            (hash
                  'id (->symbol about)
                  '_parent 'classes
                  'a (map ->symbol types) 'subclass-of (map ->symbol subclass-ofs) 'comment comments 'd descriptions 'note notes)))
    ; property
    (`(rdf:Description (@ (rdf:about ,about))
        (dcterms:description (@ (xml:lang ,_)) ,description) ...
        (dcterms:hasVersion (@ (rdf:resource ,has-version))) ...
        (dcterms:issued ,issued) ...
        (dcterms:modified ,modified) ...
        (owl:inverseOf (@ (rdf:resource ,inverse-ofs))) ...
        (rdf:type (@ (rdf:resource ,types))) ...
        (rdfs:comment (@ (xml:lang ,_)) ,comment) ...
        (rdfs:domain (@ (rdf:resource ,domains))) ...
        (rdfs:isDefinedBy (@ (rdf:resource ,is-defined-by))) ...
        (rdfs:label (@ (xml:lang ,lang)) ,creator) ...
        (rdfs:range (@ (rdf:resource ,ranges))) ...
        (rdfs:range ,range-expressions) ...
        (rdfs:seeAlso (@ (rdf:resource ,see-also))) ...
        (rdfs:subPropertyOf (@ (rdf:resource ,subproperty-ofs))) ...
        (skos:definition (@ (xml:lang ,_)) ... ,definitions) ...
        (skos:example (@ (xml:lang ,_)) ... ,examples) ...
        (skos:note (@ (xml:lang ,_)) ,notes) ...
        (skos:scopeNote (@ (xml:lang ,_)) ,scope-notes) ...
        )
          (let* (
                (about (get-pure-name about)))
          (hash
                'id (->symbol about)
                '_parent 'properties
                'a (map ->symbol types) 'range (map ->symbol ranges) 'domain (map ->symbol domains) 'subproperty-of (map ->symbol subproperty-ofs) 'comment comment 'd description 'note notes)))
    (else
      (count (+ 1 (count)))
      (when (< (count) 10)
        (--- (count))
        (---- item-sxml)
        (--- "\n"))
      #f)))

(define-catch (xml-ontology->context path ontology-id)
  (let* ((xml (read-file path))
        (sxml (ssax:xml->sxml (open-input-string xml) (hash->list general-namespaces)))
        (item-sexps
          (match sxml
            (`(*TOP* ,_ ,_ (rdf:RDF ,description ...) ,_ ...) description)
            (else empty)))
        ; order sublists by alphabet order of ids
        (item-sexps (map
                      (λ (item-sexp)
                        (sort
                          item-sexp
                          (λ (a b)
                            (cond
                              ((not (list? a)) #t)
                              ((not (list? b)) #f)
                              ((equal? (->string (car a)) "@" ) #t)
                              ((equal? (->string (car b)) "@" ) #f)
                              (else (string<?
                                      (->string (car a))
                                      (->string (car b))))))))
                      item-sexps))
        (items (and
                  item-sexps
                  (map (curryr build-item ontology-id) item-sexps)))
        (items (cleanmap items))
        (namespaces-in-use (get-namespaces-in-use items))
        (_ (--- namespaces-in-use))
        (general-namespaces (hash-delete general-namespaces ontology-id)) ; otherwise we have two items with the same id and an endless cycle
        (items (append
                  items
                  (list
                    (hash 'id 'namespaces '_parent ontology-id)
                    (hash 'id 'definitions '_parent ontology-id)
                    (hash 'id 'classes '_parent 'definitions)
                    (hash 'id 'properties '_parent 'definitions))
                  (for/list
                    (((k v) general-namespaces))
                    (hash 'id k 'xmlns v '_parent 'namespaces))))
        )
    items))
