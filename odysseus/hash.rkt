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

(define-catch (get-$ path hh)
  (cond
    ((empty? hh) empty)
    ((empty? path) hh)
    ((hash? hh) (get-$ (rest path) (hash-ref* hh (first path))))
    ((and (list? hh) (one-element? hh)) (get-$ (rest path) (hash-ref* (first hh) (first path))))
    ((list? hh)
      (let ((res (cleanmap (map (λ (h) (get-$ path h)) hh))))
        (if (one-element? res)
          (first res)
          res)))
    (else #f)))

(define-macro ($ dotted-path hh)
  (let ((path (string-split (->string dotted-path) ".")))
    `(get-$ ',path ,hh)))

; the same of hash-ref but no matter of type, matches if string projection values of hash key and compared key are equal
(define-catch (hash-ref* h key (default-value #f))
  (hash-ref
    (hash-map
      (λ (k v) (values (->string k) v))
      h)
    (->string key)
    default-value))

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
      (hash-insert res (cons k v))
      res)))

(define (hash-clean lambdakv h)
  (hash-filter (λ (k v) (not (lambdakv k v))) h))

(define hash-filter-not hash-clean)

(define (hash-delete h k)
  (cond
    ((immutable? h) (hash-remove h k))
    (else (hash-delete (make-immutable-hash (hash->list h)) k))))

(define (hash-substitute h1 arg)
  ;(printf "arg: ~a~n(car arg): ~a~nresulted hash: ~a~n" arg (car arg) (hash-delete h1 (car arg)))
  (cond
    ((null? arg) h1)
    ((simple-cons? arg)
      (hash-insert
        (hash-delete h1 (car arg))
        arg))
    ((cons-ext? arg)
      (hash-insert
        (hash-delete h1 (car arg))
        arg))
    ((list-of-cons? arg)
      (if (null? (cdr arg))
        (hash-substitute h1 (car arg))
        (hash-substitute (hash-substitute h1 (car arg)) (cdr arg))))
    (else h1)))

(define (hash-revert h)
  (apply hash (interleave (hash-values h) (hash-keys h))))

(define hash-insert hash-set)

;;;;; TODO >>>
(define (list->hash lst header #:key-index (key-index 1) #:columns-exclude (columns-exclude null))
  (let* ((header (remove-by-pos header key-index)))
    (for/hash ((i lst))
      (values
        (nth i key-index)
        (hash-remove-keys
          (apply hash (interleave header (remove-by-pos i key-index)))
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
