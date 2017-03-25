#lang racket

(require "../lib/all.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define l list)

(define ATOMS (@
                'H (l "H" 1)
                'C (l "C" 12)
                'N (l "N" 14)
                'O (l "O" 16)
                'P (l "P" 31)
                'S (l "S" 32)
                'Ca (l "Ca" 40)
                'Fe (l "Fe" 56)))

(define BONDS (list '-- '== '≡≡ '··))

(define (strip-off sym)
  (string->symbol (nth (split (symbol->string sym) "/") 1)))

(define (cyclic? sym)
  (> (length (split (symbol->string sym) "/")) 1))

;(define-macro (atom a)
;  `(nth (hash-ref ATOMS ',a "") 1))

;(define (bond link-type)
;  (λ (a b)
;    (let ((link-symbol
;            (case link-type
;              (("covalent" "ionic") "-")
;              (("covalent double") "=")
;              (("covalent triple") "≡")
;              (("hydrogen") "··")
;              (else " "))))
;    (println b)
;    (cond
;      ((list? b)
;        (format
;          "~a~a~a"
;          a
;          link-symbol
;          (implode
;            (sort
;              b
;              (λ (x y) (< (len x) (len y))))
;            ",")))
;      (else
;        (format "[~a~a~a]" a link-symbol b))))))
;
;(define -- (bond "covalent"))
;(define == (bond "covalent double"))
;(define ≡≡ (bond "covalent triple"))
;(define .. (bond "covalent triple"))
