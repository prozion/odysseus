#lang racket

(require compatibility/defmacro)
(require "base.rkt" "seqs.rkt" "type.rkt" (for-syntax "seqs.rkt"))

(provide (all-defined-out))

;; INIT
(define (@ . body)
  ;(apply hash body))
  ;(apply hash (clean nil? body)))
  (let* ((l (length body))
        (body (if (even? l) body (cdr body))))
  (make-hash
    ; remove all pairs with nil-type keys:
    (clean (λ (x) (nil? (car x))) (hash->list (apply hash body))))))

(define-macro (hash-sym . body)
  (let ((nbody (map (λ(x) (if (symbol? x) (symbol->string x) x)) body)))
    `(hash ,@nbody)))

(define (hasher-by-names . body)
  (λ
    args
    (for/hash ((k body) (v args)) (values k v))))

;; REDUCE
(define (hash-length h)
  (length (hash-keys h)))

(define (hash->alist h)
  (map
    (λ (x) (list (car x) (cdr x)))
    (hash->list h)))

(define (hash->ordered-list h keys-order)
  (for/list
    (((k v) (in-hash h))
    (i keys-order))
    (hash-ref h i #f)))

; '((foo 1 2 3 4) (bar 10 20 30)) -> #(foo:#(a:1 b:2 c:3) bar:#(a:10 b:20 c:30))
(define (list->hash lst header #:key-index (key-index 1) #:columns-exclude (columns-exclude null))
  (let* ((header (remove header key-index)))
    (for/hash ((i lst))
      (values
        (nth i key-index)
        (hash-remove-keys
          (apply hash (interleave header (remove i key-index)))
          columns-exclude)))))

;; ACCESS
; (@. h.a.b.c)
(define-macro (@. path)
  (let* ((parts (map string->symbol (split (symbol->string path) ".")))
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

(define (hash-take h n)
  (let ((hl (hash-length h)))
    (cond
      ((>= n hl) h)
      (else (for/hash
              ( ((k v) (in-hash h))
                (i (in-range hl))
                #:break (= i n))
              (values k v))))))

;; FILTER
(define (@clean . body)
  (make-hash
    ;; remove all pairs with nil-type values:
    (clean (λ (x) (nil? (cdr x))) (hash->list (apply @ body)))))

(define (hash-filter lambdakv h)
  (for/fold
    ((res (hash)))
    (((k v) h))
    (if (lambdakv k v)
      (hash-insert res (cons k v))
      res)))

(define (hash-clean lambdakv h)
  (hash-filter (λ (k v) (not (lambdakv k v))) h))

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

;; MAP
(define (hash-map f h)
  (for/hash (((k v) (in-hash h))) (f k v)))

(define (deep-hash-map f h)
  (for/hash (((k v) (in-hash h)))
    (if (hash? v)
      (values k (deep-hash-map f v))
      (f k v))))

(define map-hash hash-map)

;; MODIFY
(define (hash-delete h k)
  (cond
    ((immutable? h) (hash-remove h k))
    (else (hash-delete (make-immutable-hash (hash->list h)) k))))

(define (hash-delete-all h keys)
  (cond
    ((empty? keys) h)
    (else (hash-delete-all (hash-delete h (car keys)) (cdr keys)))))

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

(define (hash-insert h1 pair)
  (cond
    ((null? pair) h1)
    ;((list-of-simple-cons? pair) (hash-insert (hash-insert h1 (car pair)) (cdr pair))) ; doesn't catch mixed lists of cons and lists
    ((list-of-seqs? pair) (hash-insert (hash-insert h1 (car pair)) (cdr pair))) ; each element of list is either cons or list
    ((not (pair? pair)) h1)
    ((not (hash? h1)) (make-hash (list pair)))
    ((hash-ref h1 (car pair) #f) h1)
    (else
      (hash-set h1 (car pair) (cdr pair)))))

(define (hash-group-by h field)
  (for/fold
    ((s (hash)))
    (((k v) h))
    (hash-insert-fuse
      s
      (cons
        (hash-ref v field)
        (hash k v)))))

; use hash-set for hash-insert-force
; ...
(define (hash-insert-fuse h1 pair)
  (cond
    ((not (pair? pair)) h1)
    ((not (hash? h1))  (hash (car pair) (cdr pair)))
    (else
      (let ((h1-part-v (hash-ref h1 (car pair) #f)))
        (cond
          ((not h1-part-v) (make-immutable-hash (cons pair (hash->list h1))))
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

;; COMBINE
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
          (hash-insert-fuse a (cons k v)))))
    (else (hash-union (car hs) (apply hash-union (cdr hs))))))

;; OUTPUT
(define (hash-print h #:delimeter (delimeter ", ") #:prefix (prefix "") #:equal-sign (equal-sign "="))
  (let ((hl (hash-length h)))
    (for/fold
      ((s ""))
      (((k v) (in-hash h)) (i hl))
      (let ((vp (cond
                  ((string? v) (str "'" v "'"))
                  (else v))))
      (if (< i (dec hl))
        (str s prefix k equal-sign vp delimeter)
        (str s prefix k equal-sign vp))))))

(define (hash-print-json h #:prefix (prefix ""))
  (format "{~a}"
          (hash-print h #:delimeter ", " #:prefix prefix #:equal-sign ": ")))

(define (print-hash format-str h)
  (for/fold
    ([res ""])
    ([(k v) (in-hash h)])
    (string-append res (format format-str k v))))

(define (hash-remove-keys h keys)
  (cond
    ((null? keys) h)
    (else (hash-remove-keys (hash-remove h (car keys)) (cdr keys)))))
