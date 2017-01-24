#lang racket

(require compatibility/defmacro)
(require "base.rkt" "seqs.rkt" (for-syntax "seqs.rkt"))

(provide (all-defined-out))

(define (@ . body)
  ;(apply hash body))
  ;(apply hash (clean nil? body)))
  (let* ((l (length body))
        (body (if (even? l) body (cdr body))))
  (make-hash
    ; remove all pairs with nil-type keys:
    (clean (λ (x) (nil? (car x))) (hash->list (apply hash body))))))

(define (@clean . body)
  (make-hash
    ;; remove all pairs with nil-type values:
    (clean (λ (x) (nil? (cdr x))) (hash->list (apply @ body)))))

(define (map/hash f h)
  (for/hash (((k v) (in-hash h))) (values k (f v))))

(define (hash-length h)
  (length (hash-keys h)))

(define (print-hash format-str h)
  (for/fold
    ([res ""])
    ([(k v) (in-hash h)])
    (string-append res (format format-str k v))))

(define-macro (hash-sym . body)
  (let ((nbody (map (λ(x) (if (symbol? x) (symbol->string x) x)) body)))
    `(hash ,@nbody)))

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

(define (hash-refs h keys (missed null))
  (define (hash-refs-iter h keys res)
    (cond
      ((empty? keys) res)
      (else (hash-refs-iter h (cdr keys) (rpush res (hash-ref h (car keys) missed))))))
  (hash-refs-iter h keys empty))

; (@. h.a.b.c)
(define-macro (@. path)
  (let* ((parts (map string->symbol (split (symbol->string path) ".")))
        (h (car parts))
        (rest (map (λ (x) `(quote ,x)) (cdr parts))))
    (cond
      ((null? rest) h)
      (else `(hash-path ,h ,@rest)))))

(define (hash-delete h k)
  (cond
    ((immutable? h) (hash-remove h k))
    (else (make-immutable-hash (hash->list h)))))

(define (hash-insert h1 pair)
  (let ((h1-v-hash (hash-ref h1 (car pair) #f)))
    (if (and
          (hash? (cdr pair))
          h1-v-hash)
      ;; #t - we union values if they are hashes
      (hash-insert
        (hash-delete h1 (car pair))
        (cons
          (car pair)
          (if (hash? h1-v-hash)
            (hash-union h1-v-hash (cdr pair))
            h1-v-hash)))
      ;; #f - add to hash in usual way
      (make-hash (cons pair (hash->list h1))))))

(define (hash-revert h)
  (apply hash (interleave (hash-values h) (hash-keys h))))

; add to resulting hash all key-val pairs from h1 and pairs from h2 with rest of the keys
(define (hash-union h1 h2)
;; already exists in racket/hash: hash-union (make-immutable-hash <list-of pairs> ...)
  (let ((h1 (if (nil? h1) (hash) h1))
        (h2 (if (nil? h2) (hash) h2)))
    (for/fold
      ((a h1))
      (([k v] (in-hash h2)))
      (hash-insert a (cons k v)))))

(define (hasher-by-names . body)
  (λ
    args
    (for/hash ((k body) (v args)) (values k v))))

(define (hash-regex-filter reg h)
  (make-hash
    (filter
      (λ (x)
        (let* (
                (key (car x))
                (key (cond
                      ((symbol? key) (symbol->string key))
                      ((string? key) key)
                      (else #f))))
          (if key
                (regexp-match reg key)
                #f)))
      (hash->list h))))
