#lang racket

(require compatibility/defmacro)
(require "base.rkt")
(require "controls.rkt")
(require racket/set)
(require (only-in
            racket/base
            (reverse std:reverse)))

(provide (all-defined-out))

(define (list-pretty-string lst)
  (for/fold
    ((res ""))
    ((s lst))
    (string-append res s "\n")))

(define-catch (implode lst (sep ""))
  (->> lst ((swap add-between) sep) (map ~a) (apply string-append)))
  ; (apply string-append (map ~a (add-between lst sep))))

(define (interleave l1 l2)
  (define (interleave-iter l1 l2 lres)
    (cond
      ((or (null? l1) (null? l2)) lres)
      (else (interleave-iter (cdr l1) (cdr l2) (pushr (pushr lres (car l1)) (car l2))))))
  (interleave-iter l1 l2 empty))

(define (split-with f lst (true-part empty))
  (append
    true-part
    (call-with-values (λ () (splitf-at lst f)) list)))

(define (list-ref* lst index)
  (let ((len (length lst)))
    (cond
      ((< (+ len index) 0) #f)
      ((>= index len) #f)
      ((< index 0) (list-ref lst (+ len index)))
      (else (list-ref lst index)))))

; for backward code compatibility:
(define (nth lst index)
  (cond
    ((empty? lst)
      #f)
    ((= index 0)
      #f)
    ((< index 0)
      (list-ref* lst index))
    (else
      (list-ref* lst (sub1 index)))))

(define (indexof lst el (f equal?))
  (add1 (or
          (index-of lst el f)
          -1)))

(define index-of? (combine not (curry equal? #f) index-of))

(define (index-of*? lst el)
  (index-of lst el (λ (a b) (equal? (~a a) (~a b)))))

(define (count-element lst el)
  (count (curry equal? el) lst))
  ; (length (indexes-of lst el)) ; alternative

(define (regexp-index-of? lst regxp)
  (ormap
    (λ (x) (regexp-match? (pregexp regxp) x))
    lst))

; defensive take
(define-catch (take* lst (count 1))
  (let ((len (length lst)))
    (cond
      ((empty? lst)
        lst)
      ((= count 1)
        (first lst))
      ((> count (length lst))
        lst)
      (else
        (take lst count)))))

(define-catch (drop* lst (count 1))
  (let ((len (length lst)))
    (cond
      ((empty? lst)
        lst)
      ((> count (length lst))
        empty)
      (else
        (drop lst count)))))

(define (take-right* lst (count 1))
  (take-right lst count))

(define (append-elements lst . els)
  (append lst els))

(define pushr append-elements)

(define concat append)

(define (list-conj lst . els)
  (cond
    ((empty? els) lst)
    (else (apply list-conj (cons (car els) lst) (cdr els)))))

(define-catch (list-remove-by-pos lst pos)
  (cond
    ((>= pos (length lst))
      lst)
    (else
      (append (take lst pos) (drop lst (add1 pos))))))

;; slice inclusively: slice c f -> a b [c d e f] g
(define (slice lst pos1 (pos2 (length lst)))
  (let ((ll (length lst)))
    (cond
      ((empty? lst) lst)
      ((< pos1 0) (slice lst (+ ll pos1 1) pos2))
      ((< pos2 0) (slice lst pos1 (+ ll pos2 1)))
      ((> pos2 ll) (slice lst pos1))
      (else
        (drop-right
          (drop* lst (sub1 pos1))
          (- ll pos2))))))

(define (splice lst sublst pos)
  (append (take* lst pos) sublst (drop* lst pos)))

(define uniques remove-duplicates)

(define (not-uniques lst)
  (remove-duplicates
    (filter
      (λ (x) (not-empty? (cdr (indexes-of lst x))))
      lst)))

(define (minus seq1 seq2 #:equal-f (equal-f #f))
  (if equal-f
    (filter-not
       (λ (x)
              (for/or
                ((s seq2))
                (cond
                  ((and (list? x) (list? s)) (equal-f x s))
                  (else (equal? x s)))))
       seq1)
    (reverse (set-subtract seq1 seq2))))

(define (difference seq1 seq2)
  (append
     (filter-not
       (λ (x) (index-of? seq2 x))
       seq1)
     (filter-not
       (λ (x) (index-of? seq1 x))
       seq2)))

(define (unique-difference seq1 seq2)
  (remove-duplicates (difference seq1 seq2)))

(define (intersect . seqs)
  (let ((seqs (filter-not false? seqs)))
    (cond
      ((empty? seqs) empty)
      ((= (length seqs) 1) empty) ; nothing to be intersected with
      (else
        (for/fold
          ((res (car seqs)))
          ((lst (cdr seqs)))
          (reverse (set-intersect res lst)))))))

(define (intersect? . seqs)
  (not-empty? (apply intersect seqs)))

(define (equal-elements? seq1 seq2)
  (empty? (unique-difference seq1 seq2)))

(define (equal-set? seq1 seq2 (deep #f))
  ; (set=? (apply set seq1) (apply set seq2)))
  (cond
    ((empty? seq1) (empty? seq2))
    ((empty? seq2) (empty? seq1))
    ((index-of? seq2 (car seq1))
      (equal-set? (cdr seq1) (remove (car seq1) seq2)))
    (else #f)))

(define (deep-equal-set? seq1 seq2)
  (let ((search-func
          (λ (s) (or
            (equal? s (car seq1))
            (and (list? s) (list? (car seq1)) (deep-equal-set? s (car seq1)))))))
    (cond
      ((empty? seq1) (empty? seq2))
      ((empty? seq2) (empty? seq1))
      ;; consider elements equal also in the case, when they are lists and contain the same elements, no matter in what order (elements are equal-setted too) and do it in the recursive manner
      ((for/or ((s seq2)) (search-func s))
          (deep-equal-set? (cdr seq1) (filter-not search-func seq2)))
      ((index-of? seq2 (car seq1))
          (deep-equal-set? (cdr seq1) (remove* (list (car seq1)) seq2 (car seq1))))
      (else #f))))

(define (partition-all lst n)
  (define (partition-iter lst res)
    (cond
      ((< n 1) lst)
      ((empty? lst) res)
      ((< (length lst) n) (pushr res lst))
      (else
        (partition-iter
          (drop* lst n)
          (pushr
            res
            (take lst n))))))
  (partition-iter lst empty))

(define (transpose llst)
  (cond
    ((null? (cdr (car llst))) (list (flatten llst)))
    (else (append (list (flatten (map car llst))) (transpose (map cdr llst))))))

(define (cleanmap seq)
  (filter-not false? seq))

(define (append-unique . seqs)
  (remove-duplicates (apply append seqs)))

(define (last? el lst)
  (equal? el (last lst)))

(define (several? lst)
  (and lst (list? lst) (> (length lst) 1)))

(define (only-or-first x (default #f))
  (cond
    ((list? x)
      (if (not-empty? x)
        (first x)
        default))
    (else x)))

(define (sort-by-order lst given-order-v)
  (->>
    given-order-v
    (filter (λ (x) (index-of? lst x)))
    ((λ (x) (append x (minus lst given-order-v))))))
