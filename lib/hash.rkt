#lang racket

(require compatibility/defmacro)
(require "base.rkt" "seqs.rkt" "type.rkt" (for-syntax "seqs.rkt"))

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

(define (hash-pair h key (default #f))
  (for/fold
    ((res default))
    (((k v) h))
    (if (equal? k key)
        (cons k v)
        res)))

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
    (else (hash-delete (make-immutable-hash (hash->list h)) k))))

(define (hash-substitute h1 arg)
  (cond
    ((null? arg) h1)
    ((cons? arg) ; STX cons
      (hash-insert
        (hash-delete h1 (car arg))
        arg))
    ((list-of-cons? arg)
      (if (null? (cdr arg))
        (hash-substitute h1 (car arg))
        (hash-substitute (hash-substitute h1 (car arg)) (cdr arg))))
    (else h1)))

(define (hash-insert-hard h1 pair)
  (make-hash (cons pair (hash->list h1))))

(define (hash-insert h1 pair)
  (cond
    ((not (pair? pair)) h1)
    ((not (hash? h1))  (hash (car pair) (cdr pair)))
    (else
      (let ((h1-part-v (hash-ref h1 (car pair) #f)))
        (cond
          ((not h1-part-v)   (make-hash (cons pair (hash->list h1))))
          (else
            (hash-insert
              (hash-delete h1 (car pair))
              (cons
                (car pair)
                (cond
                  ((and
                      (hash? h1-part-v)
                      (hash? (cdr pair)))
                        (hash-union h1-part-v (cdr pair)))
                  ((and
                      (list? h1-part-v)
                      (list? (cdr pair)))
                        (merge-unique h1-part-v (cdr pair)))
                  (else
                    h1-part-v))))))))))

(define (hash-revert h)
  (apply hash (interleave (hash-values h) (hash-keys h))))

; add to resulting hash all key-val pairs from h1 and pairs from h2 with rest of the keys
(define (hash-union . hs)
;; already exists in racket/hash: hash-union (make-immutable-hash <list-of pairs> ...)
  (case (length hs)
    ((1) (car hs))
    ((2)
      (let* (
            (h1 (car hs))
            (h2 (cadr hs))
            (h1 (if (nil? h1) (hash) h1))
            (h2 (if (nil? h2) (hash) h2)))
        (for/fold
          ((a h1))
          (([k v] (in-hash h2)))
          (hash-insert a (cons k v)))))
    (else (hash-union (car hs) (apply hash-union (cdr hs))))))

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
