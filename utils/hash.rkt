#lang racket

(require compatibility/defmacro)
(require "base.rkt")

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

(define (hash-insert h1 pair)
  (make-hash (cons pair (hash->list h1))))

; add to resulting hash all key-val pairs from h1 and pairs from h2 with rest of the keys
(define (hash-union h1 h2)
;; already exists in racket/hash: hash-union (make-immutable-hash <list-of pairs> ...)
  (let ((h1 (if (nil? h1) (hash) h1))
        (h2 (if (nil? h2) (hash) h2)))
    (for/fold
      ((a h1))
      (([k v] (in-hash h2)))
      (hash-insert a (cons k v)))))
