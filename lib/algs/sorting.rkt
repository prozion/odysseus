#lang racket

; (define (insertion-sort a) ; array -> array
;   (for/fold
;     ((res a))
;     ((j (range+ 2 (vector-length a))))
;     (let ((key (vector-ref a j))
;           (i (- j 1)))
;       (let loop ((res2 res1) (i i))
;         (cond
;           ((and (> i 0) (> (vector-ref a i) key))
;             (let ((a (vector-set! a (+ i 1) (vector-ref a i))))
;               (loop (a (- i 1)))))
;           (else
;           (

(require "kormen-base.rkt")
(require "../load/all.rkt")

(provide (all-defined-out))

(define (bubble-sort a)
  (define (swap a i j)
    (let ((t (nth a i)))
      (setn
        (setn a i (nth a j))
        j t)))
  (for/fold
    ((res1 a))
    ((i (range+ 1 (length a))))
    (for/fold
      ((res2 res1))
      ((j (range+ i (-- (length a)))))
      (cond
        ((> (nth res2 j) (nth res2 (++ j))) (swap res2 j (++ j)))
        (else res2)))))

(define (insertion-sort-1 a) ; list -> list
  (for/fold
    ((a a))
    ((j (range+ 2 (length a))))
    (let ((key (nth a j))
          (i (-- j)))
      (let loop ((i i) (a a))
        (cond
          ((and (> i 0) (> (nth a i) key))
            (loop
              (-- i)
              (setn a (++ i) (nth a i))))
          (else (setn a (++ i) key)))))))

(define (insertion-sort-2 a) ; list -> list
  (define (insertion-sort-iter part1 key part2)
    (cond
      ((empty? part1) (pushl part2 key))
      ((> (last part1) key) (insertion-sort-iter (trimr part1) key (pushl part2 (last part1))))
      (else (append part1 (list key) part2))))
  (for/fold
    ((res a))
    ((j (range+ 2 (length a))))
    (insertion-sort-iter (shiftl res (-- j)) (nth res j) (triml res j))))

(define (merge-sort a)
  (cond
    ((empty? a) empty)
    ((empty? (cdr a)) a)
    (else
      (let* ((n (length a))
            (n1 (quotient n 2))
            (a1 (shiftl a n1))
            (n2 (- n n1))
            (a2 (shiftr a n2))
            (sorted-a1 (divide-sort a1))
            (sorted-a2 (divide-sort a2)))
        (cond
          ((empty? sorted-a1) sorted-a2)
          ((empty? sorted-a2) sorted-a1)
          (else
            (let loop ((part1 sorted-a1) (part2 sorted-a2) (res empty))
              (cond
                ((and (empty? part1) (empty? part2)) res)
                ((empty? part1) (loop part1 (cdr part2) (pushr res (car part2))))
                ((empty? part2) (loop (cdr part1) part2 (pushr res (car part1))))
                ((< (car part1) (car part2)) (loop (cdr part1) part2 (pushr res (car part1))))
                (else (loop part1 (cdr part2) (pushr res (car part2))))))))))))
