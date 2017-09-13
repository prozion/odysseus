#lang racket

(require "atoms.rkt")
(require "../lib/all.rkt")

(provide (all-defined-out))

; calculates mass of molecule
(define (mol/mass mol)
  (let* ((flatten-mol (flatten mol))
        (flatten-mol (clean
                        (λ (y) (indexof? BONDS y))
                        flatten-mol))
        (non-cyclic-atoms (clean cyclic? flatten-mol))
        (cyclic-atoms (filter cyclic? flatten-mol))
        (cyclic-atoms (map strip-off (uniques cyclic-atoms)))
        (atoms (merge non-cyclic-atoms cyclic-atoms)))
    (apply
      +
      (map
        (λ (x) (nth (hash-ref ATOMS (strip-off x)) 2))
        atoms))))

(define (mol/print mol)
  mol)

; '(O (-- H) (-- H))
(define HOH '(O (-- H) (-- H)))

; '(O (-- H))
(define -OH '(O (-- H))) ; hydroxyl
(define -CH '(C (-- H)))
(define -CH2 '(C (-- H) (-- H)))
(define -CH3 '(C (-- H) (-- H) (-- H)))

; '(C (== O) (-- -OH))
(define -COOH `(C (== O) (-- ,-OH))) ; carboxyl

(define citrate `(C (-- ,-OH)
                    (--  ,-COOH)
                    (-- C (-- H) (-- H) (--  ,-COOH))
                    (-- C (-- H) (-- H) (--  ,-COOH))))

;(define ethylene1 '(== (C (-- H) (-- H)) (C (-- H) (-- H))))
;(define ethylene2 '(C (--H) (-- H) (== (C (-- H) (-- H)))))
;(define cyclopropane `(-- ,-CH2 ,-CH2 ,-CH2))

;(define =C-C=C- `(C (--H) (-- (C (-- H) (== a))) (== (C (-- H) (-- b))))
(define benzol `(C/a (-- H)
                  (== (C (-- H)
                    (-- (C (-- H)
                      (== (C (-- H)
                        (-- (C (-- H)
                          (== (C (-- H)
                (-- C/a)))))))))))))

; implement chaining?
 ;like: (chain
 ;        (chain -OH -- C -- -COOH)
 ;        --
 ;        (chain -CH2 -- -COOH)
 ;        --
 ;        (chain -CH2 -- -COOH))
 ; or
 ;      (~~
 ;        (~~ -OH -- C+ -- -COOH)
 ;        --
 ;        (~~ -CH2+ -- -COOH)
 ;        --
 ;        (~~ -CH2+ -- -COOH))
 ; where + after the symbol of chemical element denotes a point in which chain attaches to its next link
