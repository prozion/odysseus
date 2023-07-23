#lang racket

(require (only-in
            racket/hash
            (hash-union hash-union-std)))
(require compatibility/defmacro)
(require "base.rkt" "list.rkt" "type.rkt" "regexp.rkt" "debug.rkt" "controls.rkt" "string.rkt" (for-syntax "list.rkt" "type.rkt" racket/list racket/string))

(provide (all-defined-out))

; make overwrite behavior of 'hash-union' just like 'merge' in Clojure:
(define (hash-union #:combine (combine-f (λ (v1 v2) v2)) . hs)
  (apply hash-union-std #:combine combine-f hs))

; a behaviour, widely used in the old odysseus files is conservative (c) - after merging it will keep the former value, not latter.
(define (hash-union-c . hs)
  (apply hash-union-std #:combine (λ (v1 v2) v1) hs))

(define (@ . body)
  ;(apply hash body))
  ;(apply hash (clean nil? body)))
  (let* ((l (length body))
        (body (if (even? l) body (cdr body))))
  (make-hash
    ; remove all pairs with nil-type keys:
    (clean (λ (x) (nil? (car x))) (hash->list (apply hash body))))))

(define-catch (hash-ref-path hh path (default-value #f))
  (cond
    ((empty? hh) default-value)
    ((empty? path) hh)
    ((hash? hh) (hash-ref-path (hash-ref* hh (first path)) (rest path)))
    ((and (list? hh) (one-element? hh)) (hash-ref-path (hash-ref* (first hh) (first path)) (rest path)))
    ((list? hh)
      (let ((res (cleanmap (map (λ (h) (hash-ref-path h path)) hh))))
        (if (one-element? res)
          (first res)
          res)))
    (else default-value)))

(define-macro ($ dotted-path hh)
  (let ((path (string-split (->string dotted-path) ".")))
    `(hash-ref-path ,hh ',path)))

; the same of hash-ref but no matter of type, matches if string projection values of hash key and compared key are equal
(define-catch (hash-ref* h key (default-value #f))
  (hash-ref
    (hash-map
      (λ (k v) (values (->string k) v))
      h)
    (->string key)
    default-value))

(define (hash-set* h path value)
  (cond
    ((empty? path)
      h)
    ((empty? (cdr path))
      (hash-set h (car path) value))
    ((hash-has-key? h (car path))
      (hash-set
        h
        (car path)
        (hash-set* (hash-ref h (car path)) (cdr path) value)))
    (else
      (hash-set
        h
        (car path)
        (hash-set* (hash) (cdr path) value)))))

(define (hash-update* h path updater (failure-result h))
  (cond
    ((empty? path)
      h)
    ((empty? (cdr path))
      (hash-update h (car path) updater))
    ((hash? (hash-ref h (car path) #f))
      (hash-update h (car path) (λ (v) (hash-update* v (cdr path) updater))))
    (else
      failure-result)))

; (@. h.a.b.c)
(define-macro (@. path)
  (let* ((parts (map string->symbol (string-split (symbol->string path) ".")))
        (h (car parts))
        (rest (map (λ (x) `(quote ,x)) (cdr parts))))
    (cond
      ((null? rest) h)
      (else `(hash-path ,h ,@rest)))))

; (hash 'a (hash 'aa 10 'ab 20) 'b (hash 'ba (hash 'baa 300 'bab 30)))
; (hash-path h 'b 'ba 'bab) -> 30
(define (hash-path h . rest)
  (define (hash-path-r h path)
    (cond
      ((null? (cdr path)) (hash-ref h (car path) #f))
      (else (let ((calculated-hash (hash-path-r h (cdr path)))) ; if - to handle case when a path is deeper than hash
              (if (hash? calculated-hash)
                (hash-ref calculated-hash (car path) #f)
                #f)))))
  (hash-path-r h (reverse rest)))

(define (hash-refs h keys (missed #f))
  (define (hash-refs-iter h keys res)
    (cond
      ((empty? keys) res)
      (else (hash-refs-iter
              h
              (cdr keys)
              (pushr
                res
                (let ((next-key (car keys)))
                (if (scalar? next-key)
                    (hash-ref h next-key missed)
                    (next-key h))))))))
  (hash-refs-iter h keys empty))

(define (hash-map f h)
  (for/hash
    (((k v) h))
    (f k v)))

(define (hash-map-deep f h)
  (for/hash (((k v) (in-hash h)))
    (if (hash? v)
      (values k (hash-map-deep f v))
      (f k v))))

(define (hash-filter lambdakv h)
  (for/fold
    ((res (hash)))
    (((k v) h))
    (if (lambdakv k v)
      (hash-set res k v)
      res)))

(define (hash-clean lambdakv h)
  (hash-filter (λ (k v) (not (lambdakv k v))) h))

(define hash-filter-not hash-clean)

(define (hash-delete h k)
  (cond
    ((immutable? h) (hash-remove h k))
    (else (hash-delete (make-immutable-hash (hash->list h)) k))))

(define (hash-revert h)
  (apply hash (interleave (hash-values h) (hash-keys h))))

;;;;; TODO >>>
(define-catch (list->hash lst header #:key-index (key-index 0) #:columns-exclude (columns-exclude null))
  (let* ((header (list-remove-by-pos header key-index)))
    (for/hash ((i lst))
      (values
        (list-ref i key-index)
        (hash-remove-keys
          (apply hash (interleave header (list-remove-by-pos i key-index)))
          columns-exclude)))))

(define (hash-remove-keys h keys)
  (cond
    ((null? keys) h)
    (else (hash-remove-keys (hash-remove h (car keys)) (cdr keys)))))

(define-catch (hash-keys-substitute h original-keys subst-keys)
  (cond
    ((empty? subst-keys) h)
    ((empty? original-keys) h)
    (else
      (let* ((original-key (car original-keys))
            (subst-key (car subst-keys))
            (original-value (hash-ref h original-key))
            (h (hash-union (hash subst-key original-value) (hash-delete h original-key))))
        (hash-keys-substitute h (cdr original-keys) (cdr subst-keys))))))

(define-catch (decart-hash ls1 ls2)
  (let* ((ls1 (listify ls1)))
    (for/hash
      ((l1 ls1))
      (values l1 ls2))))

(define-catch (hash-ref-some h keys (fallback-value #f))
  (cond
    ((empty? keys)
      fallback-value)
    (else
      (or
        (hash-ref h (first keys) #f)
        (hash-ref-some h (rest keys) fallback-value)))))
