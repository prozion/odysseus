#lang racket

(require (prefix-in base: "base.rkt"))
(require "seqs.rkt")
(require "strings.rkt")
(require "type.rkt")
(require "debug.rkt")
(require "hash.rkt")
(require compatibility/defmacro)
(require (for-syntax racket/list))

(provide (all-defined-out))

; (firsts '((a 2) (b 10))) -> '(a b)
(define (firsts alst)
  (cond
    ((null? alst) null)
    (else (cons (first (car alst)) (firsts (cdr alst))))))

; (seconds '((a 2) (b 10))) -> '(2 10)
(define (seconds alst)
  (cond
    ((null? alst) null)
    (else (cons (second (car alst)) (seconds (cdr alst))))))

(define-macro (dup-alist lst n)
    `(map (λ (x) (quote ,lst)) (range 1 ,n)))

(define (alookup alst key)
  (let ((pair (assoc key alst)))
    (if pair
      (cadr pair)
      #f)))

; (alist-expand '((1 2) (3 4)) '(10 20) 1) -> '((1 2 10) (1 2 20) (3 4))
(define (alist-expand base-list expander-list pos)
  (let ((pos-el (nth base-list pos)))
      (splice
        (remove base-list pos)
        (map
          (λ (x) (pushr pos-el x))
          expander-list)
        pos)))

(define (alist-flatten alst (acc (list)))
  (cond
    ((empty? alst) acc)
    (else (alist-flatten (cdr alst) (append acc (car alst))))))

(define (alist->mstring alst (frmt (dupstr "~a " (length (car alst)))))
  (implode
    (map
      (λ (x) (apply (curry format frmt) x))
      alst)
    "\n"))

(define (clist-add clst pair f)
  (let* ((i (indexof (map car clst) (car pair)))
        (old-pair (nth clst i)))
    (if (= i 0)
      (pushr clst pair)
      (insert
        (remove clst i)
        i
        (cons (car old-pair) (f (cdr old-pair) (cdr pair)))))))

(define (clist-ref clst k (not-found #f))
  (cond ((null? clst) not-found)
        ((equal? k (car (car clst))) (cdr (car clst)))
        (else (clist-ref (cdr clst) k not-found))))

(define (clist-remove clst k)
  (filter-not
    (λ (x) (equal? (car x) k))
    clst))

(define (clist-sort clst f)
  (sort
    clst
    (λ (a b) (f (car a) (cdr a) (car b) (cdr b)))))

(define (hash->sorted-clist h)
  (clist-sort (hash->list h) (λ (cara cdra carb cdrb) (> cdra cdrb))))

(define (pairwise lst1 lst2)
  (for/fold
    ((s (list)))
    ((i lst1) (j lst2))
    (pushr s (cons i j))))

(define (frequency-clist lst)
  (let* ((hash-res
          (for/fold
            ((res (hash)))
            ((el lst))
            (cond
              ((hash-ref res el #f)
                  (let* ((cnt (hash-ref res el))
                        (cnt (+ 1 cnt)))
                    (hash-union
                      (hash-delete res el)
                      (hash el cnt))))
              (else (hash-union res (hash el 1)))))))
    (sort (hash->list hash-res)
          (λ (a b)
            (cond
              ((= (cdr a) (cdr b))
                (cond
                  ((and (number? (car a)) (number? (car b)))
                    (< (car a) (car b)))
                  ((and (string? (car a)) (string? (car b)))
                    (string<? (car a) (car b)))
                  (else #t)))
              (else
                (> (cdr a) (cdr b))))))))

(module+ test

  (require rackunit)
  (require "test.rkt")
  (require "debug.rkt")

  (check-equal? (firsts '((a 2) (b 10))) '(a b))

  (check-equal? (seconds '((a 2) (b 10))) '(2 10))

  (check-equal? (alookup '((a 10) (b 20)) 'a) 10)
  (check-equal? (alookup '((a 10) (b 20) (a 30)) 'a) 10)
  (check-equal? (alookup '((a 10) (b 20)) 'c) #f)
  (check-equal? (alookup '(((a d) 10) (b 20)) '(a d)) 10)

  ; (check-speed
  ;             (alist-expand (dup-alist (1 2 3) 100) (range 1 1000) 50)
  ;             10)

  (check-equal? (alist-flatten '((a 10) (b 20))) '(a 10 b 20))
  (check-equal? (alist-flatten '((a 10) (b 20) (c (30 40)))) '(a 10 b 20 c (30 40)))
  (check-equal? (alist-flatten '((a 10))) '(a 10))
  (check-equal? (alist-flatten '()) empty)
  (check-equal? (alist-flatten '(())) empty)

  (check-equal? (clist-ref '((1 . 2) (3 . 4)) 3) 4)
  (check-equal? (clist-ref '((1 . 2) (3 . 4)) 7 10) 10)
  (check-equal? (clist-ref (list (cons 'a 'c)) 'a) 'c)
  (check-equal? (clist-ref '(("absdf" . "boo")) "absdf") "boo")
  (check-equal? (clist-ref '() 10) #f)

  (check-equal? (clist-add '((1 . 2) (3 . 4))
                            '(3 . 5)
                            (λ (v1 v2) (+ v1 v2)))
                '((1 . 2) (3 . 9)))

  (check-equal? (clist-add '((1 . 2) (3 . 4))
                            '(4 . 5)
                            (λ (v1 v2) (+ v1 v2)))
                '((1 . 2) (3 . 4) (4 . 5)))

  (check-equal? (clist-sort
                  '((1 . 3) (2 . 2) (3 . 3) (7 . 1) (4 . 2) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1))
                  (λ (k1 v1 k2 v2) (< k1 k2)))
                '((1 . 3) (2 . 2) (3 . 3) (4 . 2) (5 . 1) (7 . 1) (8 . 1) (9 . 1) (10 . 1) (44 . 1) (45 . 1)))

  (check-equal? (clist-sort
                  '((1 . 3) (2 . 2) (3 . 3) (7 . 1) (4 . 2) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1))
                  (λ (k1 v1 k2 v2) (< v1 v2)))
                '((7 . 1) (9 . 1) (10 . 1) (45 . 1) (8 . 1) (44 . 1) (5 . 1) (2 . 2) (4 . 2) (1 . 3) (3 . 3)))

  (check-equal? (pairwise '(1 2 3) '(4 5 6))
                '((1 . 4) (2 . 5) (3 . 6)))

  (check-equal? (pairwise '(1 2 3) '(4 5 6 7))
                '((1 . 4) (2 . 5) (3 . 6)))

  (check-equal? (pairwise '(1 2 3 10) '(4 5 6))
                '((1 . 4) (2 . 5) (3 . 6)))

  (check-equal? (frequency-clist '(1 1 1 1 4 5 2 9 1 6 8 0 2 4 2)) '((1 . 5) (2 . 3) (4 . 2) (0 . 1) (5 . 1) (6 . 1) (8 . 1) (9 . 1)))

)
