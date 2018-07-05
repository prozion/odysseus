#lang racket

(require (for-syntax (prefix-in seqs: "seqs.rkt") "type.rkt" racket/list))
(require "seqs.rkt")
(require "base.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "regexp.rkt")
(require "optimize.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define key-name 'id)
(define alt-key-names '(alt-id))
(define parent-key '_parent)
(define label-key '_label)

(define (key? k)
  (equal? k key-name))

(define (get-key h)
  (hash-ref h key-name))

(define get-id get-key)

(define-catch (equal-ids? id1 id2)
  (equal? (->string id1) (->string id2)))

(define-catch (get-item-key item)
  (hash-ref item key-name #f))

(define-catch (get-item-without-key item)
  (hash-delete item key-name))

(define-catch (get-parent item)
  (if item
    (hash-ref item parent-key #f)
    #f))

(define-catch (add-parent item parent)
  (hash-union (hash parent-key parent) item))

(define-catch (special-parameter? p)
  (or
    (key? p)
    (indexof? alt-key-names p)
    (re-matches? "_.*" (->string p))))

(define-catch (category? element-h)
  (empty?
    (filter-not
      special-parameter?
      (hash-keys element-h))))

(define-catch (planarize hash-tree (acc-result (list)))
  (cond
    ((hash-empty? hash-tree) acc-result)
    (else
      (planarize
        (apply opt/hash-union (hash-values hash-tree))
        (append-unique
          acc-result
          (hash-keys hash-tree))))))

;; all not special attributes without ids
(define-catch (non-special-attrs? h)
  (not-empty?
    (filter-not
      (λ (x) (or* special-parameter? key? x))
      (hash-keys h))))

(define-catch (get-special-attrs h )
  (filter special-parameter? (hash-keys h)))

(define-catch (get-non-special-attrs h #:exclude (exclude #f))
  (filter-not
    (λ (x)
            (or
              (special-parameter? x)
              (and exclude (indexof? exclude x))))
    (hash-keys h)))

; access and modification functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; find key by known id,
; gets key only from the first level
(define-catch (hash-tree-get-key-by-id hash-tree id (id-attr 'id))
  (let* ((res (filter (λ (e) (equal? (hash-ref e id-attr #f) id)) (hash-keys hash-tree)))
        (res (if (not-empty? res) (car res) #f)))
    res))

; get value by the path in the hashtree
(define-catch (hash-tree-get-value hash-tree path)
  (cond
    ((empty? path) hash-tree)
    (else (hash-tree-get-value (hash-ref hash-tree (car path)) (cdr path)))))

; get value by the id-path in the hashtree
(define-catch (hash-tree-get-value-by-id-path hash-tree id-path (id-attr 'id))
  (cond
    ((empty? id-path) hash-tree)
    (else
      (let ((key (filter
                    (λ (e) (and (hash? e) (equal? (car id-path) (hash-ref e id-attr))))
                    (hash-keys hash-tree))))
        (if (empty? key)
          #f
          (hash-tree-get-value-by-id-path (hash-ref hash-tree (car key)) (cdr id-path) id-attr))))))

; get a whole element in the key position
(define-catch (hash-tree-get-element-by-id-path hash-tree id-path)
  ; (--- id-path hash-tree)
  (cond
    ((one-element? id-path)
      (let* (
              (matched-key-element (filter
                                      (λ (x) (untype-equal? ($ id x) (car id-path)))
                                      (hash-keys hash-tree)))
              (matched-key-element (if (not-empty? matched-key-element) (car matched-key-element) #f)))
        matched-key-element))
    (else
      (let* ((next-id (first id-path))
            (next-hash-tree (hash-filter (λ (k v) (untype-equal? ($ id k) next-id)) hash-tree))
            (next-hash-tree (if (hash-empty? next-hash-tree) (error "wrong accessor path") (car (hash-values next-hash-tree)))))
        (hash-tree-get-element-by-id-path next-hash-tree (cdr id-path))))))

; sets new value to the end key element defined by the path
(define-catch (hash-tree-set-value hash-tree path value)
  (cond
    ((empty? path) value)
    (else
      (let* ((next-key (car path))
            (next-value (hash-ref hash-tree next-key)))
        (hash-union
          (hash next-key (hash-tree-set-value next-value (cdr path) value))
          hash-tree)))))

; set value by the id-path in the hashtree
(define-catch (hash-tree-set-value-by-id-path hash-tree id-path value (id-attr 'id))
  (cond
    ((empty? id-path) value)
    (else
      (let ((key (filter
                    (λ (e) (and (hash? e) (equal? (car id-path) (hash-ref e id-attr))))
                    (hash-keys hash-tree))))
        (cond
          ((empty? key) hash-tree)
          (else
            (hash-union
              (hash (car key) (hash-tree-set-value-by-id-path (hash-ref hash-tree (car key)) (cdr id-path) value id-attr))
              hash-tree)))))))

; helper function for hash-tree-add-value-by-id-path*
; id-path - direct order (a1 a2 a3)
(define-catch (find-non-existed-path-elements hash-tree id-path (id-attr 'id))
  (cond
    ((empty? id-path) empty)
    ; find mathced element to the first element of the id-path
    (else
      (let* ((key (filter
                    (λ (e) (equal? (hash-ref e id-attr) (car id-path)))
                    (hash-keys hash-tree)))
            (key (if (not-empty? key) (car key) #f)))
        (if key
            (find-non-existed-path-elements (hash-ref hash-tree key) (cdr id-path) id-attr)
            ; e.g. (a3 a2):
            id-path)))))

; helper function for hash-tree-add-value-by-id-path*
; id-path (a1 a2 a3)
(define-catch (add-id-path value id-path (id-attr 'id))
  (cond
    ((empty? id-path) value)
    (else
      (hash-union
        (hash
          (hash id-attr (car id-path)) ; key
          (add-id-path (hash) (cdr id-path) id-attr)) ; value
        value))))

; adds value by the id-path in the hashtree, if id-path doesn't exist, creates this path
; id-path - (a1 a2 a3)
(define-catch (hash-tree-add-value-by-id-path* hash-tree id-path value (id-attr 'id))
  (let ((end-path-element (hash-tree-get-value-by-id-path hash-tree id-path id-attr)))
    (if end-path-element
      ; if end element exist, just add value as usual:
      (let* ((old-value (hash-tree-get-value-by-id-path hash-tree id-path id-attr))
            (new-value (hash-union
                          (hash value (hash))
                          old-value)))
        (hash-tree-set-value-by-id-path hash-tree id-path new-value id-attr))
      ; if end element doesn't exist, create the chain to this element
      (let* (
            (non-existed-ids (find-non-existed-path-elements hash-tree id-path id-attr))
            (existed-ids (rtrim id-path (length non-existed-ids)))
            (last-existed-element-value (hash-tree-get-value-by-id-path hash-tree existed-ids id-attr))
            (new-last-existed-element-value (add-id-path last-existed-element-value non-existed-ids id-attr))
            (new-hash-tree (hash-tree-set-value-by-id-path hash-tree existed-ids new-last-existed-element-value id-attr)))
        new-hash-tree))))

; filter and hash-tree subset selection functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-catch (get-leaves hash-tree #:exclude (exclude '(name)))
  (cond
    ((not (hash? hash-tree)) empty)
    ((hash-empty? hash-tree) empty)
    (else
      (for/fold
        ((res empty))
        ((ke (hash-keys hash-tree)))
        (let* (
              (non-special-attrs (if (hash? ke)
                                  (get-non-special-attrs ke #:exclude exclude)
                                  #f))
              (res
                (cond
                  ((and non-special-attrs (not-empty? non-special-attrs)) (pushr res ke))
                  (else res))))
          (append res (get-leaves (hash-ref hash-tree ke) #:exclude exclude)))))))

(define-catch (get-item-by-id-from-the-list plained-hash-tree id (id-attr 'id))
  ; (--- "get-item-by-id-from-the-list:" ($ id plained-hash-tree) id)
  (let ((res
          (filter
            (λ (e) (equal? (hash-ref e id-attr) id))
            plained-hash-tree)))
    (cond
      ((empty? res) #f)
      (else (car res)))))

; hash -> list-of-lists
; gets  a list of all paths (sequential ids to the end-points)
(define-catch (get-paths hash-tree (curpath empty) (result empty))
  (cond
    ((hash-empty? hash-tree) (list curpath))
    (else
      (for/fold
        ((res result))
        ((k (hash-keys hash-tree)))
        (cond
          ((hash? k)
            (append-unique res (get-paths (hash-ref hash-tree k) (pushr curpath (get-key k)) res)))
          (else
            res))))))

; handy macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get key hash or value
; (define-syntax ($4 stx)
;   (syntax-case stx ()
;     ((_ path hash-tree id ...)
;         ; id ... is here to avoid two patterns and duplication of with-syntax code
;         (let* (
;               (id-lst (syntax->datum #'(id ...)))
;               (conditional-id (if (empty? id-lst) #f (car id-lst))))
;           (with-syntax ((conditional-id-stx (datum->syntax stx conditional-id))
;                         (path-lst (datum->syntax stx (seqs:split (symbol->string (syntax->datum #'path)) "."))))
;             (if conditional-id
;                 #'(hash-tree-get-element-by-id-path hash-tree 'path-lst conditional-id-stx)
;                 #'(hash-tree-get-element-by-id-path hash-tree 'path-lst)))))))
;
; (define-syntax ($3 stx)
;   (syntax-case stx ()
;     ((_ path hash-tree id ...)
;         ; id ... is here to avoid two patterns and duplication of with-syntax code
;         (let* (
;               (id-lst (syntax->datum #'(id ...)))
;               (conditional-id (if (empty? id-lst) #f (car id-lst))))
;           (with-syntax ((conditional-id-stx (datum->syntax stx conditional-id))
;                         (path-lst (datum->syntax stx (seqs:split (symbol->string (syntax->datum #'path)) "."))))
;             (if conditional-id
;                 #'(hash-tree-get-value-by-id-path hash-tree 'path-lst conditional-id-stx)
;                 #'(hash-tree-get-value-by-id-path hash-tree 'path-lst)))))))
;
; ; take first level elements under the path
; (define-macro ($3-1 path hash-tree)
;   `(hash-keys ($3 ,path ,hash-tree)))
;
; (define-macro ($3-2 path hash-tree)
;   `(flatten (map hash-keys (hash-values ($3 ,path ,hash-tree)))))

(define-catch (get-$1 path hashtree)
  (let* ((hash-item (cond ((> (length path) 1)
                            (get-$2 (but-last path) hashtree))
                          ((= (length path) 1)
                            (get-$2 path hashtree))
                          (else
                              #f))))
    (if hash-item
      (hash-ref* hash-item (last path) #f)
      #f)))

(define-catch (get-$2 path hashtree)
  (let ((res-lst
          (filter
            (λ (x) (equal-ids? ($ id x) (last path)))
            (get-$3 (but-last path) hashtree))))
    (if (empty? res-lst) #f (car res-lst))))

(define (get-$3 path hashtree)
  (sort
    (hash-keys (get-$4 path hashtree))
    (λ (a b)
      (let* ((a-order ($ _order a))
            (b-order ($ _order b)))
        (if (and a-order b-order)
          (< a-order b-order)
          #t)))))

(define (get-$4 path hashtree)
  (cond
    ((empty? path) hashtree)
    (else
      (let* ((next-id (car path))
            (key (filter (λ (x)
                            (equal-ids? ($ id x) next-id)) (hash-keys hashtree)))
            (next-hash-tree (if (not-empty? key)
                                  (hash-ref* hashtree (car key))
                                  (error (str "wrong path: " (implode path "."))))))
        (get-$4 (cdr path) next-hash-tree)))))

(define-catch (do-by-sections hashtree path-to-sections f-header-of-section f-section-content)
  (for/fold
    ((res1 ""))
    ((section (get-$3 path-to-sections hashtree)))
    (str
      res1
      (f-header-of-section section)
      (for/fold
        ((res2 ""))
        ((item (get-$3 (pushr path-to-sections ($ id section)) hashtree)))
        (str
          res2
          (f-section-content item section))))))

(define-catch (do-flat-list hashtree path-to-sections f-section-content)
  (let ((section ($2 path-to-sections hashtree))
        (items (get-$3 path-to-sections hashtree)))
    (for/fold
      ((res ""))
      ((item items))
      (str
        res
        (f-section-content item section)))))

(define-macro ($4 path hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(get-$4 (list ,@path) ,hashtree)))

(define-macro ($3 path hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(get-$3 (list ,@path) ,hashtree)))

(define-macro ($2 path hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(get-$2 (list ,@path) ,hashtree)))

(define-macro ($1 path hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(get-$1 (list ,@path) ,hashtree)))

(define-macro ($$->str path f-header-of-section f-section-content hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(do-by-sections ,hashtree (list ,@path) ,f-header-of-section ,f-section-content)))

(define-macro ($$-flat->str path f-section-content hashtree)
  (let ((path (seqs:split (->string path) ".")))
    `(do-flat-list ,hashtree (list ,@path) ,f-section-content)))
