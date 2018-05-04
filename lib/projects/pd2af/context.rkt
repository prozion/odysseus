#lang racket

(require "common.rkt")
(require "geometry.rkt")
(require "sexp.rkt")
(require "types.rkt")
(require "../../load/all.rkt")

(provide (all-defined-out))

(define (%% context sexp)
  (hash 'context context 'sexp sexp))

(define (%%-context pd)
  (hash-ref pd 'context))

(define (%%-sexp pd)
  (hash-ref pd 'sexp))

(define (element-type element) (hash-ref element 'type #f))

(define (element-id element)
  ($ id element))

(define (build-name name uoi)
  (let* ((cardinality (get-first-group-match "N:([0-9]+)" uoi))
        (postfix-name (case cardinality
                        (("1") "")
                        (("2") "_dimer")
                        (("3") "_trimer")
                        (("4") "_tetramer")
                        (("5") "_pentamer")
                        (("6") "_hexomer")
                        (("7") "_heptamer")
                        (("8") "_octomer")
                        (("9") "_enneamer")
                        (("10") "_decamer")
                        (else "_multimer"))))
    (str name postfix-name)))

(define-catch (name-by-id id context)
  ($ name ($$->el id context)))

(define-catch (incoming-arcs? id context (d #f))
  (ormap
    (λ (element)
      ; (when (and d (and (or* ProcessNode? Arc? ActivityArc? element) (indexof? ($ targets element) id)))
      ;     (--- id ($ id element) ($ targets element)))
      (and (or* ProcessNode? Arc? ActivityArc? element) (indexof? ($ targets element) id)))
    context))

(define-catch (degree id context)
  (length
    (uniques
      (filter (λ (element)
              (or
                (indexof? ($ targets element) id)
                (indexof? ($ sources element) id)))
          context))))

(define-catch (not-same-name-incoming-arcs? el-id context (d #f))
  (let* ((el ($$->el el-id context))
        (el-name ($ name el))
        (el-sources (filter (λ (e) (equal? ($ target e) el-id)) context))
        (el-sources-not-same-name (filter-not (λ (s) (equal? (name-by-id s context) el-name)) el-sources)))
    (ormap
      (λ (element)
        (and
          (or* ProcessNode? Arc? ActivityArc? element)
          (indexof? el-sources-not-same-name ($ id element))))
      context)))

(define-catch (p-ctx context #:id (id #f) #:show (show #f))
  (let ((res (if id
                (filter
                  (λ (x) (equal? ($ id x) id))
                  context)
                context)))
    (if show
          (print-list (map (λ (x)
                              (map (λ (i)
                                      (hash-ref x i #f))
                                    show))
                              res))
          (print-list res))))

(define-catch (context-redirect old-ids new-id context)
  ; (when (equal? old-id "glyph49.2")
  (for/fold
    ((res context))
    ((old-id old-ids))
    (for/list
      ((el res))
      (let ((sources ($ sources el))
            (targets ($ targets el)))
        (cond
          ((indexof? sources old-id)
            (hash-substitute el (cons 'sources (replace sources old-id new-id))))
          ((indexof? targets old-id)
            (hash-substitute el (cons 'targets (replace targets old-id new-id))))
          (else el))))))

;; context modification
(define-catch (remove-from-context-by-id context id)
  (let* ((context
          (for/fold
            ((res empty))
            ((el context))
            (let* ((sources ($ sources el))
                  (targets ($ targets el))
                  (in-sources? (and sources (indexof? sources id)))
                  (in-targets? (and targets (indexof? targets id))))
            (cond
              ((equal? ($ id el) id) res)
              (in-sources?
                (if (one-element? sources)
                  (remove-from-context-by-id res ($ id el))
                  (pushr res
                    (hash-union
                      (hash 'sources (exclude sources id))
                      el))))
              (in-targets?
                (if (one-element? targets)
                  (remove-from-context-by-id res ($ id el))
                  (pushr res
                    (hash-union
                      (hash 'targets (exclude targets id))
                      el))))
              (else (pushr res el))))))
        (context (filter-not (λ (el) (or
                                        (empty? ($ sources el))
                                        (empty? ($ targets el))))
                              context)))
    context))

  ; (filter-not (λ (x) (equal? ($ id x) id)) context))

; remove from context by id
(define ($$-- id context)
  (remove-from-context-by-id context id))

; add/substitute <hash-part> at element with <id>
(define-catch ($$-> id hash-part context)
  (let* ((element ($$->el id context)))
      (cond
        ((not element) context)
        (else
          ($$++ (hash-union hash-part element) (exclude context element))))))

; substitute old-id element to new-id element
(define-catch ($$<> old-id new-id context)
  ; (---- (filter (λ (el) (indexof? (list old-id new-id) ($ id el))) context))
  (let* (
        (old-element ($$->el old-id context))
        (old-sources ($ sources old-element))
        (old-targets ($ targets old-element))
        (new-element ($$->el new-id context))
        ; (_ (when (not new-element) (error (format "~a doesn't exist in the context") new-id)))
        (new-sources ($ sources new-element))
        (new-targets ($ targets new-element))
        (joint-sources (and old-sources new-sources (uniques (append old-sources new-sources))))
        (joint-targets (and old-targets new-targets (uniques (append old-targets new-targets))))
        ; exclude old element from context
        (context (exclude context old-element))
        ; if no new-id element exists in the context -- create a clone of old-id element, with new-id id.
        (context (if (not new-element)
                        ($$++
                          (hash-union (hash 'id new-id) old-element)
                          context)
                        context))
        ; in the case, element is Arc - redirect all links, that lead from el outside
        (context (cond
                    ((or (not-empty? joint-sources) (not-empty? joint-targets))
                      ($$-> new-id (hash 'sources joint-sources 'targets joint-targets) context))
                    (else
                      context)))
        ; substitute old-id to new-id in all Arc that income or outcome from the old element, if the element is a Node
        (context (for/fold
                  ((res empty))
                  ((e context))
                  (let* ((e-sources ($ sources e))
                        (e-sources (if e-sources
                                      (replace e-sources old-id new-id)
                                      #f))
                        (e-targets ($ targets e))
                        (e-targets (if e-targets
                                      (replace e-targets old-id new-id)
                                      #f)))
                    (cond
                      ((or e-sources e-targets) (pushr res (hash-union (hash 'sources e-sources 'targets e-targets) e)))
                      (else (pushr res e)))))))
    context))

; add, if new
(define-catch ($$++ el context)
  (pushr-unique context el))

; sources -arc- el -arc- targets
; return source and target neighbours for a given node
(define-catch (-$- el context)
  (let* ((el-id ($ id el))
        (arcs (filter
                (λ (x) (and ($ sources x) ($ targets x)))
                context))
        (in-arcs (filter
                    (λ (x) (indexof? ($ targets x) el-id))
                    arcs))
        (out-arcs (filter
                    (λ (x) (indexof? ($ sources x) el-id))
                    arcs))
        (sources (map
                    (λ (x)
                      (map
                        (λ (y) ($$->el y context))
                        ($ sources x)))
                    in-arcs))
        (targets (map
                    (λ (x)
                      (map
                        (λ (y) ($$->el y context))
                        ($ targets x)))
                    out-arcs))
        (sources (opt/uniques (flatten sources)))
        (target (opt/uniques (flatten targets))))
    ; (--- sources el-id target)
    (hash 'sources sources 'targets target)))

(define (reduce-ids context nodes-grouped)
  (for/fold
    ((res (list)))
    ((group nodes-grouped))
    (pushr res (get-element-by-id (car group) context))))

(define (only-connecting-arcs arcs sexp)
	(let ((existing-ids (opt/uniques (flatten sexp))))
		(filter
			(λ (arc) (indexof existing-ids ($ id arc)))
			arcs)))

(define (get-arc-id source-id target-id glyphs)
  (let ((matched-glyphs
          (filter
            (λ (el)
                    (and
                      (indexof? ($ sources el) source-id)
                      (indexof? ($ targets el) target-id)))
            glyphs)))
    (if (empty? matched-glyphs)
      #f
      ($ id (car matched-glyphs)))))

(define (get-context-parameter context parname (default #f))
	(let ((parameters
					(filter
						(λ (x) (equal? ($ class x) "parameters"))
						context)))
		(hash-ref (car parameters) parname default)))

(define (what-kind-of-element el context)
	(let* ((context-sources (map
														(λ (x) ($ sources x))
														(filter
															(λ (y) ($ sources y))
															context)))
				(context-targets (map
													(λ (x) ($ targets x))
													(filter
														(λ (y) ($ targets y))
														context)))
				(id ($ id el)))
; #:source-end? (source-end? #f) #:target-end? (target-end? #f) #:mixed-end? (mixed-end #f) #:single-end? (single-end? #f))
		(hash
			'source-end?
			(and
				(for/or ((sources-set context-sources))
						(indexof? sources-set id)) ; somewhere it is source
				(for/and ((targets-set context-targets))
						(not (indexof? targets-set id)))) ; nowhere it is target
			'target-end?
			(and
				(for/or ((targets-set context-targets))
						(indexof? targets-set id)) ; somewhere it is target
				(for/and ((sources-set context-sources))
						(not (indexof? sources-set id)))) ; nowhere it is source
			'mixed-end?
			(for/or
        ((element-bunches (append context-targets context-sources)))
				(and
          (not-empty? (filter-not (λ (b-id) (ElementaryEPN? ($$->el b-id context))) element-bunches))
          (indexof? element-bunches id)))
			'single-end?
			(for/or
        ((element-bunches (append context-targets context-sources)))
				(and
          (indexof? element-bunches id)
          (one-element? element-bunches)))
			'one-action?
      (one-element?
        (filter
          (λ (e) (indexof? e id))
          (append context-targets context-sources)))
)))

;;;;;;;; AF
(define (to-af-class pd-class)
  (case pd-class
    (("process" "association" "dissociation") "positive influence")
    (("catalysis" "stimulation") "positive influence")
    (("modulation") "uknown influence")
    (("inhibition") "negative influence")
    (("necessary stimulation") "necessary stimulation")
    (else
      ; (--- pd-class)
      "unknown op")))

(define (inverse-class af-class)
  (case af-class
    (("positive influence") "negative influence")
    (("negative influence") "positive influence")
    (else
      af-class)))

(define (create-af-element af-context element)
  (pushr af-context element))

(define (add-af-element pd-context af-context element-id)
  (if ($$->el element-id af-context) ; if we have already added af element to af-context
    af-context
    (let* (
          (element ($$->el element-id pd-context))
          (af-element (cond
                                ((ProcessNode? element) (hash 'id element-id 'class "positive influence" 'sources ($ sources element) 'targets ($ targets element)))
                                ((Node? element) (hash 'id element-id  'class "biological activity" 'type ($ class element) 'compartment ($ compartment element) 'name ($ name element)
                                                        'x ($ x element) 'y ($ y element) 'w W 'h H
                                                        'uoi (or ($ uoi element) "")))
                                ((ModulationArc? element) (hash 'id element-id 'class (to-af-class ($ class element)) 'sources ($ sources element)  'targets ($ targets element)))
                                (else #f)))
          )
      ($$++ af-element af-context))))

(define (add-af-elements pd-context af-context . element-ids)
  (let ((element-ids (flatten element-ids)))
    (for/fold
      ((res af-context))
      ((element-id element-ids))
      (add-af-element pd-context res element-id))))

;
(define (another-same-name+class-element? el af-context)
  (let* ((compartment ($ compartment el))
        (id ($ id el))
        (name ($ name el))
        (type (or ($ type el) ($ class el)))
        (uoi ($ uoi el))
        (nodes (exclude (filter ActivityNode? af-context) el)))
    (ormap
      (λ (x)
              (and
                (not (equal? ($ id x) id))
                (equal? ($ compartment x) compartment)
                (equal? ($ name x) name)
                (equal? (or ($ type x) ($ class x)) type)))
      nodes)))
