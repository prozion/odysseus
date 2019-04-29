#lang racket

(require sxml)
(require "../lib/_all.rkt")
(require "../tabtree-format/tab-tree.rkt")

(provide (all-defined-out))

(define general-namespaces
  (list
    (cons 'owl "http://www.w3.org/2002/07/owl#")
    (cons 'rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (cons 'rdfs "http://www.w3.org/2000/01/rdf-schema#")
    (cons 'skos "http://www.w3.org/2004/02/skos/core#")
    (cons 'dcterms "http://purl.org/dc/terms/")
    (cons 'dcam "http://purl.org/dc/dcam/")))

(define-catch (read-tabtree-ontology path)
  (let* ((extension (get-file-extension path))
        (context (cond
                    ((equal? extension "mtree") (parse-tab-mtree path))
                    ((equal? extension "tree") (parse-tab-tree path))
                    (else (error (format "wrong ontology file extension: ~a" extension)))))
        )
    context))

(define-catch (strip-namespace val)
  (let* ((val (->string val))
        (val-parts (string-split val "/")))
    (last val-parts)))

(define-catch (build-item item-sxml)
  (match item-sxml
    ; class
    (`(rdf:Description (@ (rdf:about ,about))
        (rdfs:label (@ (xml:lang ,lang)) ,label)
        (rdfs:comment (@ (xml:lang ,_)) ,comment)
        (dcterms:description (@ (xml:lang en-US)) ,description) ...
        (rdfs:isDefinedBy (@ (rdf:resource ,is-defined-by)))
        (dcterms:issued ,issued)
        (dcterms:modified ,modified) ...
        (rdf:type (@ (rdf:resource ,type)))
        (dcterms:hasVersion (@ (rdf:resource ,has-version)))
        (rdfs:seeAlso (@ (rdf:resource ,see-also))) ...
        (rdfs:subClassOf (@ (rdf:resource ,subclass-of))) ...
        ,_ ...
        )
          (let* ((about (strip-namespace about)))
            (hash 'id (->symbol about) 'a type 'subclass-of subclass-of 'comment comment 'description description)))
    ; property
    (`(rdf:Description (@ (rdf:about ,about))
        (rdfs:label (@ (xml:lang ,lang)) ,creator)
        (rdfs:comment (@ (xml:lang ,_)) ,comment) ...
        (dcterms:description (@ (xml:lang en-US)) ,description) ...
        (rdfs:domain (@ (rdf:resource ,domains))) ...
        (rdfs:isDefinedBy (@ (rdf:resource ,is-defined-by)))
        (dcterms:issued ,issued)
        (dcterms:modified ,modified) ...
        (rdf:type (@ (rdf:resource ,type)))
        (dcterms:hasVersion (@ (rdf:resource ,has-version)))
        (rdfs:range (@ (rdf:resource ,ranges))) ...
        (rdfs:subPropertyOf (@ (rdf:resource ,subproperty-ofs))) ...
        ,_ ...
        )
          (hash 'id (->symbol about) 'a type 'range ranges 'domain domains 'subproperty-of subproperty-ofs 'comment comment 'description description))
    (else #f)))



(define-catch (read-xml-ontology path)
  (let* ((xml (read-file path))
        (sxml (ssax:xml->sxml (open-input-string xml) general-namespaces))
        (descriptions
          (match sxml
            (`(*TOP* ,_ ,_ (rdf:RDF ,description ...) ,_ ...) description)
            (else empty)))
        (items (and
                  descriptions
                  (map build-item descriptions)))
        )
    ; (---- items)
    #t))

; (define sample
; '(*TOP*
;   ; the*NAMESPACES* block is appended by SAX (general-namespaces)
;   (@
;     (*NAMESPACES*
;       (owl http://www.w3.org/2002/07/owl#)
;       (rdf http://www.w3.org/1999/02/22-rdf-syntax-ns#)
;       (rdfs http://www.w3.org/2000/01/rdf-schema#)
;       (skos http://www.w3.org/2004/02/skos/core#)
;       (dcterms http://purl.org/dc/terms/)
;       (dcam http://purl.org/dc/dcam/)))
;   (*PI* xml version="1.0" encoding="UTF-8")
;   (rdf:RDF
;     (rdf:Description (@ (rdf:about "http://purl.org/dc/terms/"))
;       (dcterms:title (@ (xml:lang en-US)) "DCMI Namespace for metadata terms in the http://purl.org/dc/terms/ namespace")
;       (rdfs:comment "To comment on this schema, please contact dcmifb@dublincore.org.")
;       (dcterms:publisher (@ (xml:lang en-US)) "The Dublin Core Metadata Initiative")
;       (dcterms:modified "2008-01-14"))
;     (rdf:Description (@ (rdf:about "http://purl.org/dc/terms/title"))
;       (rdfs:label (@ (xml:lang en-US)) "Title")
;       (dcterms:description (@ (xml:lang en-US)) "A name given to the resource.")
;       (rdfs:isDefinedBy (@ (rdf:resource "http://purl.org/dc/terms/")))
;       (dcterms:issued "2008-01-14")
;       (dcterms:modified "2008-01-14")
;       (rdf:type (@ (rdf:resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")))
;       (dcterms:hasVersion (@ (rdf:resource "http://dublincore.org/usage/terms/history/#titleT-001")))
;       (skos:note (@ (xml:lang en-US)) "In current practice, this term is used primarily with literal values; however, there are important uses with non-literal values as well.  As of December 2007, the DCMI Usage Board is leaving this range unspecified pending an investigation of options.")
;       (rdfs:subPropertyOf (@ (rdf:resource "http://purl.org/dc/elements/1.1/title"))))
;     (rdf:Description (@ (rdf:about "http://purl.org/dc/terms/creator"))
;       (rdfs:label (@ (xml:lang en-US)) "Creator")
;       (rdfs:comment (@ (xml:lang en-US)) "An entity primarily responsible for making the resource.")
;       (dcterms:description (@ (xml:lang en-US)) "Examples of a Creator include a person, an organization, or a service. Typically, the name of a Creator should be used to indicate the entity.")
;       (rdfs:isDefinedBy (@ (rdf:resource "http://purl.org/dc/terms/")))
;       (dcterms:issued "2008-01-14")
;       (dcterms:modified "2008-01-14")
;       (rdf:type (@ (rdf:resource "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")))
;       (dcterms:hasVersion (@ (rdf:resource "http://dublincore.org/usage/terms/history/#creatorT-001")))
;       (rdfs:range (@ (rdf:resource "http://purl.org/dc/terms/Agent")))
;       (rdfs:subPropertyOf (@ (rdf:resource "http://purl.org/dc/elements/1.1/creator")))
;       (rdfs:subPropertyOf (@ (rdf:resource "http://purl.org/dc/terms/contributor"))))
;     (rdf:Description (@ (rdf:about "http://purl.org/dc/terms/TGN"))
;       (rdfs:label (@ (xml:lang en-US)) "TGN")
;       (rdfs:comment (@ (xml:lang en-US)) "The set of places specified by the Getty Thesaurus of Geographic Names.")
;       (rdfs:isDefinedBy (@ (rdf:resource "http://purl.org/dc/terms/")))
;       (dcterms:issued "2000-07-11")
;       (dcterms:modified "2008-01-14")
;       (rdf:type (@ (rdf:resource "http://purl.org/dc/dcam/VocabularyEncodingScheme")))
;       (dcterms:hasVersion (@ (rdf:resource "http://dublincore.org/usage/terms/history/#TGN-003")))
;       (rdfs:seeAlso (@ (rdf:resource "http://www.getty.edu/research/tools/vocabulary/tgn/index.html")))))))
