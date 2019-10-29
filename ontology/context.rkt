#lang racket

(require sxml)
(require compatibility/defmacro)
(require "../lib/_all.rkt")
(require "../tabtree/tab-tree.rkt")
(require "common.rkt")
(require racket/syntax)

(provide (all-defined-out))


;;; ACCESS CONTEXT
(define get-element-by-id
  (memoize
    (λ (id context)
      (let ((matched-elements
              (filter
                (λ (el) (equal? ($ id el) id))
                context)))
        (if (empty? matched-elements)
          #f
          (car matched-elements))))))

; find item in the context that matches given value 1) by id 2) by any of the given keys
(define-catch (&& id context (keys #f))
  (cond
    ((or (not keys) (empty? keys))
      (get-element-by-id id context))
    ((scalar? keys)
      (let ((res (filter
                    (λ (el) (equal? (hash-ref el keys #f) id))
                    context)))
        (if (empty? res) #f (car res))))
    ((list? keys)
      (or (&& id context (car keys)) (&& id context (cdr keys))))
    (else #f)))

; add, if new
(define-catch (&&++ el context)
  (pushr-unique context el))

; add/substitute <hash-part> at element with <id>
(define-catch (&&-> id hash-part context)
  (let* ((element (&& id context)))
      (cond
        ((not element) context)
        (else
          (&&++ (hash-union hash-part element) (exclude context element))))))

;;; SORT CONTEXT
(define-catch (get-printable-keys item)
  (map ->symbol
    (filter-not
      (λ (key) (or
                  (re-matches? "_.*?" key)
                  (equal? "id" key)))
      (map ->string (hash-keys item)))))

(define-catch (sort-context-items items (patterns #f))
  (define (in-patterns? id1 id2)
    (ormap
      (λ (pattern-sequence)
        (and
          (indexof? pattern-sequence id1)
          (indexof? pattern-sequence id2)))
      patterns))
  (define (pattern-ids<? id1 id2)
    (ormap
      (λ (pattern-sequence)
        (and
          (indexof? pattern-sequence id1)
          (indexof? pattern-sequence id2)
          (< (indexof pattern-sequence id1) (indexof pattern-sequence id2))))
      patterns))
  (sort
    items
    (λ (a b)
      (let* ((a-id ($ id a))
            (b-id ($ id b)))
          (cond
            ((in-patterns? a-id b-id)
              (pattern-ids<? ($ id a) ($ id b)))
            (else
              (string<? (->string ($ id a)) (->string ($ id b)))))))))

(define-catch (sort-context context (root-id #f))
  (let* (
        (root (and root-id (&& root-id context)))
        (first-level-items (filter (λ (item) (equal? ($ _parent item) root-id)) context))
        (first-level-items-sorted (sort-context-items first-level-items '((namespaces definitions) (classes properties instances))))
        (result (for/fold
                  ((result (if root
                              (list root)
                              empty)))
                  ((fli first-level-items-sorted))
                  (let* ((fli-id ($ id fli)))
                    (cond
                      ((not fli-id) (errorf "no id key for ~a" fli))
                      (else
                        (append
                          result
                          (sort-context context fli-id))))))))
    result))

(define-catch (context->mstring context)
  (define path (make-parameter empty))
  (define (order-keys-by-default-order keys)
    (sort keys
          (λ (a b) (cond
                      ((and
                        (indexof? keys-order a)
                        (indexof? keys-order b))
                          (< (indexof keys-order a) (indexof keys-order b)))
                      ((indexof? keys-order a) #f)
                      ((indexof? keys-order b) #t)
                      (else #f)))))
  (let* (
        (sorted-context (sort-context context)))
    (for/fold
      ((res ""))
      ((item sorted-context))
      (let* (
            (printable-keys (get-printable-keys item))
            (printable-keys (order-keys-by-default-order printable-keys))
            (new-string (for/fold
                          ((res2 (format "~a" ($ id item))))
                          ((printable-key printable-keys))
                          (let* ((chunk-values (hash-ref item printable-key empty))
                                (chunk-values (cond
                                                ((empty? chunk-values)
                                                    #f)
                                                ((symbol? chunk-values)
                                                    (replace-namespace-for-alias chunk-values))
                                                ((scalar? chunk-values)
                                                    (->> quotate-if-sentence remove-extra-whitespaces mstring->string ->string chunk-values))
                                                ((and (list? chunk-values) (andmap symbol? chunk-values))
                                                    (implode
                                                      (cleanmap (map replace-namespace-for-alias chunk-values))
                                                      ","))
                                                (else
                                                    (implode
                                                        (map (-> quotate-if-sentence remove-extra-whitespaces mstring->string ->string) chunk-values)
                                                        ",")))))
                            (if chunk-values
                              (format "~a ~a" res2 (format "~a:~a" printable-key (string-trim chunk-values)))
                              res2))))
            ; calculate tabs:
            (parent-id ($ _parent item))
            (current-path (path))
            (current-path (cond
                            ((empty? current-path)
                                (list parent-id))
                            ((equal? (last current-path) parent-id)
                                current-path)
                            ((indexof? current-path parent-id)
                                (lshift current-path (indexof current-path parent-id)))
                            (else
                              (append current-path (list parent-id))))))
        (path current-path)
        (if (empty-string? res)
          (format "~a" new-string)
          (format "~a~n~a~a"
                      res
                      (dupstr "\t" (dec (length current-path))) new-string))))))
