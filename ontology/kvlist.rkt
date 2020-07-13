#lang racket

(require "../lib/_all.rkt")
(require compatibility/defmacro)
(require (for-syntax racket/string))

(provide (all-defined-out))

; (define-macro (kvlist->hash kvl)
;   (define (parse-values vals)
;     (cond
;       ((string? vals) (string-split vals ","))
;       (else vals)))
;   (let ((parameters (cdr kvl)))
;     (println parameters)
;     `(let* ((id (car ,kvl))
;           (parameters (cdr ,kvl))
;           )
;         #t)))

; function to read statements about a subject from s-expression, like
; (Moscow capital-of:Russia founded:1147 languages:ru,en,ar,ge,fr)
(define-catch (kvlist->hash kvl)
  ; (define (parse-values vals)
  ;   (cond
  ;     ((string? vals) (string-split vals ","))
  ;     (else vals)))
    (let* ((id (car kvl))
          (parameters (cdr kvl))
          (hash1 (hash 'id id))
          (hash2 (if (not-empty? parameters)
                                (for/fold ((res (hash)))
                                          ((p (map ->string parameters)))
                                          (let* ((key-values (string-split (->string p) ":"))
                                                (key (->symbol (car key-values)))
                                                (vals (if (not-empty? (cdr key-values)) (cdr key-values) #f))
                                                (vals (and vals (->string (car vals)))))
                                            (hash-insert-fuse res (cons key vals))))
                                (hash)))
          )
    (hash-union hash1 hash2)))

(module+ test

  (require rackunit)
  (require "../lib/checks.rkt")

  (check-hash-equal?
    (kvlist->hash '(COX-1 macromolecule name:COX_1 q:3-1))
    (hash 'id 'COX-1 'macromolecule #f 'name "COX_1" 'q "3-1"))

  (check-hash-equal?
    (kvlist->hash '(foobar variants:"foo","bar","baz" number:3))
    (hash 'id "foobar" 'variants (list "foo" "bar" "baz") 'number "3"))

  (check-hash-equal?
    (kvlist->hash '(Moscow population:11920000 country:"Russia" languages:ru,en,ta))
    (hash 'id "Moscow" 'population "11920000" 'country "Russia" 'languages (list "ru" "en" "ta")))
)
