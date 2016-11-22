#lang racket

(require compatibility/defmacro)
(require "base.rkt" (for-syntax "seqs.rkt"))

(provide (all-defined-out))

(define (@ . body)
  (apply hash (clean nil? body)))

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
              (if (hash? calculated-hash) ; STX hash? and other type predicates
                (hash-ref calculated-hash (car path) #f)
                #f)))))
  (hash-path-r h (reverse rest)))

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

; add to resulting hash all key-val pairs from h1 and pairs from h2 with rest of the keys
(define (hash-union h1 h2)
;; already exists in racket/hash: hash-union (make-immutable-hash <list-of pairs> ...)
  (let ((h1 (if (nil? h1) (hash) h1))
        (h2 (if (nil? h2) (hash) h2)))
    (for/fold
      ((a h1))
      (([k v] (in-hash h2)))
      (hash-insert a (cons k v)))))
