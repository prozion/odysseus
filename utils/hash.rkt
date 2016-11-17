#lang racket

(require compatibility/defmacro)
(require (for-syntax ))

(provide (all-defined-out))

(define @ hash)

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
      ((null? (cdr path)) (hash-ref h (car path)))
      (else (hash-ref (hash-path-r h (cdr path)) (car path)))))
  (hash-path-r h (reverse rest)))
