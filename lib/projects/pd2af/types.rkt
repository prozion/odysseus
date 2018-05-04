#lang racket

(require compatibility/defmacro)
(require "../../load/all.rkt")
(require "common.rkt")
; (require "../../../../odysseus/lib/load/all.rkt")

(provide (all-defined-out))

(define Context (list "namespace"))

; element
;   glyph
;     tag
;     equivalence_arc
;     compartment
;     node
;       process_node
;       empty_set
;       entity_pool_node
;     arc
;       flux_arc
;       modulation_arc
;       logic_arc
;       multiarc

(define (Class? aset)
  (λ (epn (context #f))
    (cond
      ((context-element? epn) (indexof? aset ($ class epn)))
      ; ((indexof? aset (class->string epn)) #t)
      (context (indexof? aset ($ class ($$->el epn context))))
      (else #f))))

(define (class->string class)
  (string-replace (->string class) "-" " "))

(define (ClassByForm? aset)
  (λ (form)
    (cond
      ((scalar? form)
        ; (--- form "--" aset "--" (class->string form) "--" (indexof? aset (class->string form)))
        (indexof? aset (class->string form)))
      ((list? form)
        ; (--- form "--" aset "--" (map class->string form) "--" (not (empty? (intersect aset (map class->string form)))))
        (not (empty? (intersect aset (map class->string form)))))
      (else #f))))

;;;;;;;;;;;;;;;;;;;; PD glyphs by classes
(define Container (list "compartment")) (define Container? (Class? Container))
(define ReferenceNode (list "tag"))

(define ElementaryEPN (list "unspecified entity" "simple chemical" "simple chemical multimer" "source and sink")) (define ElementaryEPN? (Class? ElementaryEPN))
(define NonElementaryEPN (list "macromolecule" "macromolecule multimer" "nucleic acid feature" "nucleic acid feature multimer" "perturbing agent" "complex" "complex multimer")) (define NonElementaryEPN? (Class? NonElementaryEPN))
(define Complex (list "complex")) (define Complex? (Class? Complex))
(define EPN (append ElementaryEPN NonElementaryEPN)) (define EPN? (Class? EPN)) ; Entity Pool Node
(define NamedEPN (minus EPN '("source and sink" "complex"))) (define NamedEPN? (Class? NamedEPN)) ; Entity Pool Node
(define UnnamedEPN '("source and sink" "complex")) (define UnnamedEPN? (Class? UnnamedEPN)) ; Entity Pool Node
(define SourceSink '("source and sink")) (define SourceSink? (Class? SourceSink))
(define EmptySet (list "empty set"))
(define ProcessNode (list "process" "omitted process" "uncertain process" "association" "dissociation")) (define ProcessNode? (Class? ProcessNode))
(define LogicalOperator (list "and" "or" "not")) (define LogicalOperator? (Class? LogicalOperator))
(define Node (append EPN EmptySet ProcessNode LogicalOperator)) (define Node? (Class? Node))

(define FluxArc (list "consumption" "production")) (define FluxArc? (Class? FluxArc))
(define ModulationArc (list "modulation" "stimulation" "catalysis" "inhibition" "necessary stimulation")) (define ModulationArc? (Class? ModulationArc))
(define LogicArc (list "logic arc")) (define LogicArc? (Class? LogicArc))
(define EquivalenceArc empty)
(define MultiArc (list "multiarc")) (define MultiArc? (Class? MultiArc))
(define Arc (append FluxArc ModulationArc LogicArc EquivalenceArc MultiArc)) (define Arc? (Class? Arc))
(define ArcForm (append ProcessNode ModulationArc))
    (define ArcForm? (ClassByForm? ArcForm))

(define ExtendedGlyph (append MultiArc))

(define ProcessGlyph (append ReferenceNode EquivalenceArc Node Container Arc ExtendedGlyph)) (define ProcessGlyph? (Class? ProcessGlyph))

;;;;;;;;;;;;;;;;;;;; AF glyphs by classes
(define ActivityNode (list "biological activity" "phenotype" "perturbation")) (define ActivityNode? (Class? ActivityNode))
(define ActivityDefaultTypes (list "macromolecule" "macromolecule multimer"))
(define ActivityArc (list "unknown influence" "positive influence" "negative influence" "necessary stimulation" "logic arc" "equivalence arc"))
    (define ActivityArc? (Class? ActivityArc))
    (define ActivityArcForm? (ClassByForm? ActivityArc))
(define ActivityLogicalOperator (list "and" "or" "not")) (define ActivityLogicalOperator? (Class? ActivityLogicalOperator))
(define ActivityGlyph (append ActivityNode ActivityArc ActivityLogicalOperator)) (define ActivityGlyph? (Class? ActivityGlyph))

;;;;;;;;;;;;;;;;;;;; Categories of glyphs
(define (InternalEPN? el) ($ complex el))
(define (Def? el) ($ def el))
(define (Env? el) ($ env el))
(define NonDefaultEPN (minus EPN '("macromolecule" "macromolecule multimer" "source and sink"))) (define NonDefaultEPN? (Class? NonDefaultEPN)) ; Entity Pool Node
