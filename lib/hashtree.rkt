#lang racket

(require (for-syntax (prefix-in seqs: "seqs.rkt") "type.rkt" racket/list))
(require "seqs.rkt")
(require "base.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "regexp.rkt")
(require "strings.rkt")
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

(define-catch (get-special-attrs h)
  (filter special-parameter? (hash-keys h)))

(define-catch (get-non-special-attrs h #:exclude (exclude #f))
  (filter-not
    (λ (x)
            (or
              (special-parameter? x)
              (and exclude (indexof? exclude x))))
    (hash-keys h)))

(define-catch (hashtree->string
                  h
                  #:order-key (order-key #f)
                  #:attributes (attributes #f)
                  #:exclude-lines? (exclude-lines? #f)
                  #:conversion-table (conversion-table #f))
  (define-catch (hashtree->string-iter h curlevel res-acc order-key)
    (cond
      ((hash-empty? h) "")
      (else
        (let* ((ks (hash-keys h))
              (ks (if order-key
                      (sort ks (λ (a b) (let* ((aval (hash-ref a order-key 0))
                                              (bval (hash-ref b order-key 0)))
                                          (cond
                                            ((and (number? aval) (number? bval))
                                                (< aval bval))
                                            (else
                                              (string<? (->string aval) (->string bval)))))))
                      ks))
              (k (car ks))
              (v (hash-ref h k))
              (h-rest (hash-delete h k))
              (_ (when (not (hash? k)) (error "Any leaf must be a hash in the hashtree")))
              (_ (when (not ($ id k)) (error "Any item in the hashtree must contain 'id' key")))
              (id ($ id k))
              (other-keys (exclude (hash-keys k) 'id))
              (tabs (dupstr "\t" curlevel))
              (new-line (format "~a~a ~a\n" tabs id (hash->string (hash-delete k 'id)  #:delimeter " " #:equal-sign ":" #:attributes attributes #:conversion-table conversion-table)))
              (res (cond
                      ((and exclude-lines? (exclude-lines? k)) res-acc)
                      (else (string-append res-acc new-line))))
              (res (if (hash-empty? v)
                      res
                      (hashtree->string-iter v (+ 1 curlevel) res order-key)))
              (res (if (hash-empty? h-rest)
                      res
                      (hashtree->string-iter h-rest curlevel res order-key)))
              )
          res))))
  (hashtree->string-iter h 0 "" order-key))

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
(define-catch (get-leaves hash-tree
                  #:exclude (exclude '(name keys-order))
                  ; attributes that derived from root items to the leaves:
                  #:derived-attrs (derived-attrs empty)
                  ; hash, where keys are names of existed derived attributes and values are actual values of derived attributes:
                  #:derived-attr-values (derived-attr-values (hash)))
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
              (local-derived-attrs (cond
                                      ((list? derived-attrs) (intersect derived-attrs non-special-attrs))
                                      ((procedure? derived-attrs) (filter derived-attrs non-special-attrs))
                                      (else (errorf "wrong type of 'derived-attrs' function argument, which has the following value: ~a" derived-attrs))))
              (local-derived-attr-values (for/hash ((k local-derived-attrs)) (values k (hash-ref* ke k))))
              (derived-attr-values (hash-union local-derived-attr-values derived-attr-values))
              (new-ke (hash-union derived-attr-values ke))
              (res
                (cond
                  ((and non-special-attrs (not-empty? non-special-attrs)) (pushr res new-ke))
                  (else res))))
          (append res (get-leaves
                        (hash-ref hash-tree ke)
                        #:exclude exclude
                        #:derived-attrs derived-attrs
                        #:derived-attr-values derived-attr-values)))))))

(define-catch (get-item-by-id-from-the-list plained-hash-tree id (id-attr 'id) #:one-of? (one-of? #t))
  ; (--- "get-item-by-id-from-the-list:" ($ id plained-hash-tree) id)
  (let ((res
          (filter
            (λ (e)
                    (cond
                      (one-of? (indexof*? (string-split (->string (hash-ref* e id-attr)) ",") id))
                      (else
                        (equal? (hash-ref* e id-attr) id))))
            plained-hash-tree)))
    (cond
      ((empty? res) #f)
      (else (car res)))))

(define (@id id plained-hash-tree #:error (err #f) #:attr (attr 'id))
  (let ((res (get-item-by-id-from-the-list plained-hash-tree id attr)))
    (when (and err (not res)) (error err))
    res))

; get element by id from the planarized hashtree
(define ($$ id plained-hash-tree)
  (get-item-by-id-from-the-list plained-hash-tree id))

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

(define (hash-tree-remove-by-id h id)
  (hash-delete-f h (λ (key) (equal? ($ id key) id))))

(define (get-root-item hashtree)
  (let ((keys (hash-keys hashtree)))
    (and (not-empty? keys) (car keys))))

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
  (let ((leaves (get-$3 (but-last path) hashtree)))
    (and
      leaves
      (let ((res-lst
              (filter
                (λ (x) (equal-ids? ($ id x) (last path)))
                leaves)))
        (if (empty? res-lst) #f (car res-lst))))))

(define-catch (get-$3 path hashtree)
  (let ((selected-key-value (get-$4 path hashtree)))
    (and
      selected-key-value
      (sort
        (hash-keys selected-key-value)
        (λ (a b)
          (let* ((a-order ($ _order a))
                (b-order ($ _order b)))
            (if (and a-order b-order)
              (< a-order b-order)
              #t)))))))

(define-catch (get-$4 path hashtree)
  (cond
    ((empty? path) hashtree)
    (else
      (let* ((next-id (car path))
            (key (filter (λ (x)
                            (equal-ids? ($ id x) next-id)) (hash-keys hashtree)))
            (next-hash-tree (if (not-empty? key)
                                  (hash-ref* hashtree (car key))
                                  #f)))
                                  ; (error (str "wrong path: " (implode path "."))))))
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

(define (get-level-under path tabtree level-down)
  (let* ((path (split (->string path) "."))
        (next-leaves (get-$3 path tabtree))
        (next-ids (map (λ (x) ($ id x)) next-leaves))
        (next-paths (map (λ (x) (pushr path x)) next-ids)))
    (cond
      ((< level-down 1) tabtree)
      ((= level-down 1) next-leaves)
      (else
        (apply
          append
          (map
            (λ (next-path) (get-level-under (implode next-path ".") tabtree (- level-down 1)))
            next-paths))))))

; get the next level of leaves
(define-macro (get-level-under-1 path tabtree)
  (let ((path (seqs:split (->string path) ".")))
    `(let ((categories (get-$3 (list ,@path) ,tabtree)))
        (apply
          append
          (map
            (λ (x)
              (get-$3 (pushr (list ,@path) ($ id x)) ,tabtree))
              categories)))))

;;; manipulations with parts of the tree
(define-catch (hash-tree-remove hash-tree id-path)
  (cond
    ((empty? id-path) hash-tree)
    ((empty? (cdr id-path)) (hash-tree-remove-by-id hash-tree (car id-path)))
    (else (hash-union
            (hash-tree-remove-by-id hash-tree (car id-path))
            (hash
              (get-$2 (list (car id-path)) hash-tree)
              (hash-tree-remove (get-$4 (list (car id-path)) hash-tree) (cdr id-path)))))))

(define-catch (hash-tree-insert hash-tree id-path hash-tree-part)
  (cond
    ((empty? id-path) hash-tree)
    ((empty? (cdr id-path))
      (let* ((last-key (car id-path))
            (final-hash-subtree (get-$4 (list last-key) hash-tree))
            (final-hash-key (get-$2 (list last-key) hash-tree))
            (inserted-key (car (hash-keys hash-tree-part)))
            (inserted-val (hash-ref hash-tree-part inserted-key)))
        (if final-hash-subtree
            (hash-union
              (hash-delete hash-tree final-hash-key)
              (hash final-hash-key (hash-union final-hash-subtree hash-tree-part)))
            (hash-union
              (hash 'id last-key)
              hash-tree-part))))
    (else
      (let* ((next-key (car id-path))
            (next-hash-key (get-$2 (list next-key) hash-tree))
            (next-hash-subtree (get-$4 (list next-key) hash-tree)))
        (if next-hash-subtree
          (hash-union
            (hash-delete hash-tree next-hash-key)
            (hash next-hash-key (hash-tree-insert next-hash-subtree (cdr id-path) hash-tree-part)))
          (hash-union
            hash-tree
            (hash next-hash-key hash-tree-part)))))))

(define-catch (insert-leaf hash-tree id-path leaf)
  (let* ((inserted-hash-tree (hash leaf (hash))))
    (hash-tree-insert hash-tree id-path inserted-hash-tree)))

(define-catch (insert-leaves hash-tree id-path leaves)
  (let* ((inserted-hash-tree (for/hash ((leaf leaves)) (values leaf (hash)))))
    (hash-tree-insert hash-tree id-path inserted-hash-tree)))

(define-catch (make-hash-tree root-name list-of-hash)
  (hash
    (hash 'id root-name)
    (for/hash
      ((item list-of-hash))
      (values item (hash)))))

; get items that keep pointers to their children (rather than parents in the usual case)
; (define-catch (get-forward-tree hash-tree)
;   (define (get-forward-tree-0 root-item items)
;     (let* ((children (filter (λ (item) (equal? ($ id root-item) ($ _parent item))) items))
;           (children-ids (map (λ (item) ($ id item)) children)))
;       (cond
;         ((not-empty? children) (pushl
;                                   (apply append (map (curryr get-forward-tree-0 items) children))
;                                   (hash-set root-item '_children children-ids)))
;         (else (list (hash-set root-item '_children empty))))))
;   (let* (
;         (items (planarize hash-tree))
;         (root-elements (filter (λ (item) (not ($ _parent item))) items)))
;     (apply append (map (curryr get-forward-tree-0 items) root-elements))))

(module+ test

  (require rackunit)
  (require "checks.rkt")
  (require "type.rkt")
  (require "debug.rkt")

(define hash-tree-1 (hash
                      (hash 'id "category 1")
                        (hash
                          (hash 'id "a" 'value "1")
                            (hash)
                          (hash 'id "b" 'value "2")
                            (hash
                              (hash 'id "b1" 'value "10")
                              (hash))
                          (hash 'id "c" 'value "3")
                            (hash))
                      (hash 'id "category 2" 'status "inactive")
                        (hash
                          (hash 'id "d" 'value "-1")
                            (hash) )
                      ))

(define hash-tree-2 (hash
                      (hash 'id "root1")
                        (hash
                          (hash 'id "a" 'value "1")
                            (hash)
                          (hash 'id "b" 'value "2")
                            (hash
                              (hash 'id "b1" 'value "10")
                              (hash)
                              (hash 'id "b2" 'value "30")
                              (hash))
                          (hash 'id "c" 'value "3")
                            (hash))
                      (hash 'id "root2" 'status "inactive")
                        (hash
                          (hash 'id "d" 'value "-1")
                            (hash) )
                      ))

  (check-true (category? (hash 'id "category 1")))
  (check-true (category? (hash 'id "category 1" '_parent #f)))
  (check-false (category? (hash 'id "category 1" 'foo "bar")))
  (check-false (category? (hash 'id "c" 'value "3")))

  (check-same-elements? (planarize hash-tree-1)
                        (list
                          (hash 'id "category 1")
                          (hash 'id "a" 'value "1")
                          (hash 'id "b" 'value "2")
                          (hash 'id "b1" 'value "10")
                          (hash 'id "c" 'value "3")
                          (hash 'id "category 2" 'status "inactive")
                          (hash 'id "d" 'value "-1")))


  (check-equal? (hash-tree-get-value
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2 b21))
                      40)

  (check-equal? (hash-tree-get-value-by-id-path
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2 b21))
                      #f)

  (check-equal? (hash-tree-get-value-by-id-path
                        (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                              (hash 'id 'y) (hash
                                              (hash 'id 'z 'foo 0) 30
                                              'b2 (hash 'b21 40 'b22 50)))
                        '(y z))
                      30)

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "b" "b1"))
                      (hash))

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "b"))
                      (hash
                        (hash 'id "b1" 'value "10")
                        (hash)))

  (check-hash-equal? (hash-tree-get-value-by-id-path hash-tree-1 '("category 1" "a"))
                      (hash))

  (check-hash-equal? (hash-tree-set-value
                        (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 (hash 'b21 40 'b22 50)))
                        '(b b2)
                        100)
                      (hash 'a (hash 'a1 10 'a2 20) 'b (hash 'b1 30 'b2 100)))

  (check-hash-equal?
                    (hash-tree-set-value-by-id-path
                        (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                              (hash 'id 'y) (hash
                                              (hash 'id 'z 'foo 0) 30
                                              'b2 (hash 'b21 40 'b22 50)))
                        '(y z)
                        200)
                    (hash (hash 'id 'x) (hash 'a1 10 'a2 20)
                          (hash 'id 'y) (hash
                                          (hash 'id 'z 'foo 0) 200
                                          'b2 (hash 'b21 40 'b22 50))))

  (check-same-elements?
                (get-leaves hash-tree-1)
                (list
                  (hash 'id "a" 'value "1")
                  (hash 'id "b" 'value "2")
                  (hash 'id "b1" 'value "10")
                  (hash 'id "c" 'value "3")
                  (hash 'id "category 2" 'status "inactive")
                  (hash 'id "d" 'value "-1")))

  (check-same-elements?
                (get-leaves hash-tree-1 #:exclude '(status))
                (list
                  (hash 'id "a" 'value "1")
                  (hash 'id "b" 'value "2")
                  (hash 'id "b1" 'value "10")
                  (hash 'id "c" 'value "3")
                  (hash 'id "d" 'value "-1")))

  (check-hash-equal?
    (get-item-by-id-from-the-list
      (list (hash 'id "a") (hash 'id "b" 'value "1") (hash 'id "c" 'value "2"))
      "c")
    (hash 'id "c" 'value "2"))

  (check-same-elements?
    (get-paths hash-tree-1)
    '(("category 1" "a") ("category 1" "b" "b1") ("category 1" "c") ("category 2" "d")))

  (check-hash-equal? ($4 root1.b hash-tree-2)
                      (hash
                        (hash 'id "b1" 'value "10")
                        (hash)
                        (hash 'id "b2" 'value "30")
                        (hash)))

  (check-same-elements?
    ($3 root1 hash-tree-2)
    (list
      (hash 'id "a" 'value "1")
      (hash 'id "b" 'value "2")
      (hash 'id "c" 'value "3")))
  (check-same-elements?
    ($3 root1.b hash-tree-2)
    (list
      (hash 'id "b1" 'value "10")
      (hash 'id "b2" 'value "30")))

  (check-hash-equal? ($2 root1.b.b1 hash-tree-2) (hash 'id "b1" 'value "10"))

  (check-equal? ($1 root1.b.b1.value hash-tree-2) "10")

  (define planarized-hashtree (list (hash 'id 1 'value 10) (hash 'id 2 'value 5 '_parent 1) (hash 'id 3 'value "abc" '_parent 2)))

  (check-hash-equal? (@id 2 planarized-hashtree) (hash 'id 2 'value 5 '_parent 1))
  (check-equal? (@id 10 planarized-hashtree) #f)

  (check-hash-equal?
                (hash-tree-remove hash-tree-1 (list "category 1" "b"))
                (hash
                  (hash 'id "category 1")
                    (hash
                      (hash 'id "a" 'value "1")
                        (hash)
                      (hash 'id "c" 'value "3")
                        (hash))
                  (hash 'id "category 2" 'status "inactive")
                    (hash
                      (hash 'id "d" 'value "-1")
                        (hash) )
                  ))

  (check-hash-equal?
                (hash-tree-insert
                  hash-tree-1
                  (list "category 1")
                  (hash
                    (hash 'id "bb" 'value "10")
                      (hash
                        (hash 'id "bb1" 'value "100")
                        (hash))))
                (hash
                  (hash 'id "category 1")
                    (hash
                      (hash 'id "a" 'value "1")
                        (hash)
                      (hash 'id "bb" 'value "10")
                        (hash
                          (hash 'id "bb1" 'value "100")
                          (hash))
                      (hash 'id "b" 'value "2")
                        (hash
                          (hash 'id "b1" 'value "10")
                          (hash))
                      (hash 'id "c" 'value "3")
                        (hash))
                  (hash 'id "category 2" 'status "inactive")
                    (hash
                      (hash 'id "d" 'value "-1")
                        (hash))))

  (check-hash-equal?
                (hash-tree-insert
                  hash-tree-1
                  (list "category 2" "d")
                  (hash
                    (hash 'id "e" 'value "14")
                      (hash
                        (hash 'id "ee" 'value "325")
                          (hash))))
                (hash
                  (hash 'id "category 1")
                    (hash
                      (hash 'id "a" 'value "1")
                        (hash)
                      (hash 'id "b" 'value "2")
                        (hash
                          (hash 'id "b1" 'value "10")
                            (hash))
                      (hash 'id "c" 'value "3")
                        (hash))
                  (hash 'id "category 2" 'status "inactive")
                    (hash
                      (hash 'id "d" 'value "-1")
                        (hash
                          (hash 'id "e" 'value "14")
                            (hash
                              (hash 'id "ee" 'value "325")
                                (hash))))))

  (check-hash-equal?
                (make-hash-tree 'root (list
                                        (hash 'id 'a 'value 10)
                                        (hash 'id 'b 'value 20)))
                (hash
                  (hash 'id 'root)
                  (hash
                    (hash 'id 'a 'value 10) (hash)
                    (hash 'id 'b 'value 20) (hash))))

)
