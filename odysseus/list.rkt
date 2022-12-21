#lang racket

; (provide tostring len str list-pretty-string implode interleave explode split split-with nth indexof indexof? indexof*? regexp-indexof? count-element lshift shiftl lpop ltrim triml push lpush pushl lpush-unique pushl-unique rshift shiftr rpop rtrim trimr rpush pushr rpush-unique pushr-unique slice merge merge-unique concat splice exclude exclude-all exclude* exclude-all* insert setn replace replace-all list-substitute uniques not-uniques minus difference unique-difference intersect intersect? equal-elements? equal-set? deep-equal-set? partition-full partition-all break-lst depth transpose cleanmap merge soft-merge replace-by-part remove-by-part remove-by-pos append-unique by-index first-or-false last? several? reverse partition-by-shift vector-ref* only-or-first)

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

(define (implode lst (sep ""))
  (apply string-append (add-between lst sep)))

(define (interleave l1 l2)
  (define (interleave-iter l1 l2 lres)
    (cond
      ((or (null? l1) (null? l2)) lres)
      (else (interleave-iter (cdr l1) (cdr l2) (rpush (rpush lres (car l1)) (car l2))))))
  (interleave-iter l1 l2 empty))

(define (split-by lst (sep null))
  (cond
    ((empty? lst) empty)
    (else
      (let ((seppos (indexof lst sep)))
        (if (= seppos 0)
          (list lst)
          (push
            (lshift lst (sub1 seppos))
            (split-by (ltrim lst seppos) sep)))))))

(define (split-by-lambda-general lst f #:inclusive? (inclusive? #f))
  (let-values
    (((a b c d)
      (for/fold
        ((res (list)) (head (list)) (borders (list)) (rest lst))
        ((el lst))
        (cond
          ((f el) (values (if (empty? borders) (pushr res head) res) (list) (pushr borders el) (cdr rest)))
          ((equal? (cdr rest) null)
            (values (pushr (if (and inclusive? (not (empty? borders))) (pushr res borders) res) (pushr head el)) null null null))
          (else
            (let ((res (if (and inclusive? (not (empty? borders))) (pushr res borders) res)))
              (values res (pushr head el) (list) (cdr rest))))))))
    a))

(define (split-by-lambda lst f)
  (split-by-lambda-general lst f))

(define (split-by-lambda* lst f)
  (split-by-lambda-general lst f #:inclusive? #t))

(define (split-with f lst (true-part empty))
  (cond
    ((empty? lst) (list true-part lst))
    ((f (first lst)) (split-with f (rest lst) (pushr true-part (first lst))))
    (else (list true-part lst))))

(define (reverse lst)
  (cond
    ((list? lst) (std:reverse lst))
    ((string? lst) (implode (std:reverse (string-explode lst))))
    (else lst)))

(define (nth lst index)
  (let ((ll (length lst)))
    (cond
      ((string? lst) (if (nil? lst)
                        ""
                        (nth (split lst) index)))
      ((null? lst) null)
      ((= index 0) null)
      ((not (in (- ll) ll index)) null)
      ((< index 0) (nth lst (+ ll index 1)))
      ((= index 1) (car lst))
      (else
        ;(nth (cdr lst) (sub1 index)))))) ; slow version
        (list-ref lst (sub1 index))))))

(define (nth-cycled lst index)
  (let ((remnd (remainder index (length lst))))
    (cond
      ((>= (length lst) index) (nth lst index))
      ((= remnd 0) (nth lst -1))
      (else
        (nth lst (remainder index (length lst)))))))

(define (indexof lst el (compare-f equal?))
  (cond
    ((string? lst)
      (indexof (string-explode lst) el compare-f))
    ((list? lst)
      (let ((indexes
              (for/fold
                      ((res empty))
                      ((e lst) (i (in-naturals)))
                      (if (compare-f e el)
                        (pushr res (+ 1 i))
                        res))))
        (if (empty? indexes) 0 (car indexes))))
    (else 0)))

(define (indexof? lst el (compare-f equal?))
  (if (= 0 (indexof lst el compare-f)) #f #t))

(define (indexof*? lst el)
  (indexof? lst el (λ (a b) (equal? (~a a) (~a b)))))

(define (indexof-all lst el)
  (define (indexof-all-iter lst el acclist el-passed)
    (let ((index (indexof lst el)))
      (cond
        ((= index 0) acclist)
        (else (indexof-all-iter
                (ltrim lst (+ 1 index))
                el
                (rpush acclist (+ index el-passed))
                (+ index el-passed 1))))))
  (indexof-all-iter lst el empty 0))

(define (count-element lst el)
  (length (filter (curry equal? el) lst)))

(define (regexp-indexof? lst regxp)
  (ormap
    (λ (x) (regexp-match? (pregexp regxp) x))
    lst))

(define (lshift lst (count 1))
  (cond
    ((nil? lst) lst)
    ((string? lst) (implode (lshift (string-explode lst) count)))
    ((< count 1) empty)
    ((= count 1) (list (car lst)))
    ((> count (length lst)) lst)
    (else
      ;(cons (car lst) (lshift (cdr lst) (- count 1))))))
      (take lst count))))

(define shiftl lshift)

(define (lpop lst)
  (if (string? lst)
    (lpop (string-explode lst))
    (car lst)))

; (define first lpop)

(define (ltrim lst (count 1))
  (cond
    ((string? lst) (implode (ltrim (string-explode lst) count)))
    ((null? lst) null)
    ((>= count (length lst)) empty)
    ((< count 0) empty)
    ((= count 0) lst)
    (else
      ;(ltrim (cdr lst) (sub1 count)))))
      (list-tail lst count))))

(define triml ltrim)

(define (lpush lst . els)
  (cond
    ((empty? els) lst)
    (else
      (apply
        (curry
          lpush
          (cons (car els) lst))
        (cdr els)))))

(define pushl lpush)

(define (op-unique op)
  (λ (lst el)
    (if (indexof? lst el)
      seq
      (op lst el))))

(define lpush-unique (op-unique lpush))
(define pushl-unique (op-unique pushl))

(define (rshift lst (count 1))
  (reverse (lshift (reverse lst) count)))

(define shiftr rshift)

(define (rpop lst)
  (if (string? lst)
    (rpop (string-explode lst))
    (car (reverse lst))))

(define (rtrim lst (count 1))
  (if (string? lst)
    (implode (rtrim (split lst) count))
    (reverse (ltrim (reverse lst) count))))

(define trimr rtrim)

(define (rpush lst . els)
  (cond
    ((empty? els) lst)
    (else
      (apply
        (curry
          rpush
          (reverse (cons (car els) (reverse lst))))
        (cdr els)))))

(define pushr rpush)

(define rpush-unique (op-unique rpush))
(define pushr-unique (op-unique pushr))

;; slice inclusively: slice c f -> a b [c d e f] g
(define (slice lst pos1 (pos2 (len lst)))
  (let ((ll (length lst)))
    (cond
      ((nil? lst) lst)
      ((< pos1 0) (slice lst (+ ll pos1 1) pos2))
      ((< pos2 0) (slice lst pos1 (+ ll pos2 1)))
      ((> pos2 ll) (slice lst pos1))
      (else
        (rtrim
          (ltrim lst (sub1 pos1))
          (- ll pos2))))))

(define (merge . seqs)
  ;(define (merge2 seq1 seq2)
  ;  (if (and (string? seq1) (string? seq2))
  ;    (string-append seq1 seq2)
  ;    (if (null? seq1)
  ;      seq2
  ;      (merge2 (reverse (cdr (reverse seq1))) (cons (car (reverse seq1)) seq2)))))
  ;(case (length seqs)
  ;  ((0) empty)
  ;  ((1) (car seqs))
  ;  ((2) (merge2 (car seqs) (cadr seqs)))
  ;  (else (merge2 (car seqs) (apply merge (cdr seqs))))))
  (cond
    ((andmap string? seqs) (apply string-append seqs))
    (else (apply append seqs))))

(define (merge-unique . seqs)
  (define (merge-unique-couples-iter seq1 seq2)
    (cond
      ((null? seq2) seq1)
      ((not (list? seq2)) seq1)
      (else
        ;(merge-unique-couples-iter (rpush-unique seq1 (car seq2)) (cdr seq2)))))
        (merge seq1 (minus seq2 seq1)))))
  (define (merge-unique-iter seq1 seq2 rest-seqs)
    (cond
      ((null? rest-seqs) (merge-unique-couples-iter seq1 seq2))
      (else (merge-unique-iter (merge-unique-couples-iter seq1 seq2) (car rest-seqs) (cdr rest-seqs)))))
  (let ((l (length seqs)))
    (cond
      ((= l 0) null)
      ((= l 1) (car seqs))
      ((= l 2) (merge-unique-couples-iter (car seqs) (cadr seqs)))
      (else (merge-unique-iter (car seqs) (cadr seqs) (cddr seqs))))))

(define (push . body)
  (case (length body)
    ((0) empty)
    ((1) (car body))
    ((2) (lpush (cadr body) (car body)))
    (else (lpush (apply push (cdr body)) (car body)))))

(define (concat . body)
  body)

(define (splice lst sublst pos)
  (if (string? lst)
    (implode (splice (string-explode lst) (string-explode sublst) pos))
    (merge (lshift lst (sub1 pos)) sublst (ltrim lst (sub1 pos)))))

(define (remove-by-pos lst pos1 (pos2 pos1) #:len (size 1))
  (let ((ll (len lst)))
    (cond
      ((nil? lst) lst)
      ;((!= pos1 pos2) (merge (rtrim lst (- ll pos2)) (ltrim lst pos1)))
      ((not (in (- ll) ll pos1)) lst)
      ((not (in (- ll) ll pos2)) lst)
      ((> size 1) (remove-by-pos lst pos1 (+ pos1 (sub1 size))))
      ((< pos1 0) (cond
                    ((list? lst) (append (take lst (+ ll pos1)) (cdr (drop lst (+ ll pos1)))))
                    ((string? lst) (implode (remove-by-pos (string-explode lst) pos1)))
                    (else lst)))
      ; (remove
      ;               seq
      ;               (+ ll pos1 1)
      ;               (if (= pos2 pos1) (+ ll pos1 1) pos2)))
      ((< pos2 0) (remove-by-pos lst pos1 (+ ll pos2 1)))
      (else
        (merge
          (lshift lst (sub1 pos1))
          (rshift lst (- ll pos2)))))))

(define (exclude lst el #:compare (compare equal?))
  (if (string? lst)
    (implode (exclude (string-explode lst) el #:compare compare))
    (remove-by-pos lst (indexof lst el compare))))

(define (exclude-all lst el)
  (let ((index (indexof lst el)))
    (cond
      ((> index 0) (exclude-all (exclude lst el) el))
      (else lst))))

(define (exclude* lst . el)
  (cond
    ((empty? el) lst)
    (else
      (apply
        (curry exclude* (exclude lst (car el)))
        (cdr el)))))

(define (exclude-all* lst . el)
  (cond
    ((empty? el) lst)
    (else
      (apply
        (curry exclude-all* (exclude-all lst (car el)))
        (cdr el)))))

(define (insert lst index el)
  (let ((ll (length lst)))
    (cond
      ((= index 0) lst)
      ((string? lst)
        (implode (insert (string-explode lst) index el)))
      ((empty? lst) (list el))
      ((or (= index -1) (= index (add1 ll)))
        (rpush lst el))
      ((in (- ll) -1 index) (insert lst (+ ll index 2) el))
      ((< index 0) (insert lst 1 el))
      ((> index (add1 ll)) lst)
      (else
        (merge
          (lshift lst (sub1 index))
          (list el)
          (rshift lst (- ll (sub1 index))))))))

(define (setn lst index newel)
  (cond
    ((nil? lst) lst)
    ((> index (length lst)) lst)
    (else (insert (remove-by-pos lst index) index newel))))

(define (setns lst indexes newel)
  (cond
    ((empty? indexes) lst)
    (else (setns
            (setn lst (car indexes) newel)
            (cdr indexes)
            newel))))

(define (replace lst oldel newel)
  (setn lst (indexof lst oldel) newel))

(define (replace-all lst oldel newel)
  (cond
    ((> (indexof lst oldel) 0) (replace-all (replace lst oldel newel) oldel newel))
    (else lst)))

(define (list-substitute lst target insert)
  (cond
    ((not (list? lst)) lst)
    (else
      (for/fold
        ((acc (list)))
        ((l lst))
        (cond
          ((equal? l target) (pushr acc insert))
          ((list? l) (pushr acc (list-substitute l target insert)))
          (else (pushr acc l)))))))

; (define (uniques lst)
;   (define (uniques-iter lst acc)
;     (cond
;       ((nil? lst) acc)
;       (else (uniques-iter
;               (ltrim lst)
;               (rpush-unique acc (first lst))))))
;   (uniques-iter lst null))

(define (uniques lst)
  (cond
    ((list? lst) (remove-duplicates lst))
    ((string? lst) (uniques (string-explode lst)))
    (else lst)))

(define (not-uniques lst)
  (define (not-uniques-iter lst acc)
    (cond
      ((nil? lst) acc)
      (else (not-uniques-iter
              (ltrim lst)
              (if (znil? (indexof (ltrim lst) (first-lst lst)))
                acc
                (rpush acc (first-lst lst)))))))
  (not-uniques-iter lst null))

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

;(define (minus/hash seq1 seq2)

(define (difference seq1 seq2)
  (append
     (filter-not
       (λ (x) (indexof? seq2 x))
       seq1)
     (filter-not
       (λ (x) (indexof? seq1 x))
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
  (cond
    ((empty? seq1) (empty? seq2))
    ((empty? seq2) (empty? seq1))
    ((indexof? seq2 (car seq1))
      (equal-set? (cdr seq1) (exclude seq2 (car seq1))))
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
      ((indexof? seq2 (car seq1))
          (deep-equal-set? (cdr seq1) (exclude seq2 (car seq1))))
      (else #f))))

;; complex structures access
(define (nlist-ref lst indexes)
  (cond
    ((= (length indexes) 1) (list-ref lst (car indexes)))
    (else (nlist-ref (list-ref lst (car indexes)) (cdr indexes)))))

(define (partition-full lst n)
  (define (partition-iter lst res)
    (cond
      ((< (length lst) n) res)
      ((< n 1) lst)
      (else
        (partition-iter
          (ltrim lst n)
          (pushr
            res
            (lshift lst n))))))
  (partition-iter lst empty))

(define (partition-all lst n)
  (define (partition-iter lst res)
    (cond
      ((< n 1) lst)
      ((empty? lst) res)
      ((< (length lst) n) (pushr res lst))
      (else
        (partition-iter
          (ltrim lst n)
          (pushr
            res
            (lshift lst n))))))
  (partition-iter lst empty))

  (define (break-lst lst break-points)
    (define (break-seq-it res-acc seq-rest break-points)
      (cond
        ((empty? break-points) (pushr res-acc seq-rest))
        (else (break-seq-it
                (pushr res-acc (lshift seq-rest (car break-points)))
                (ltrim seq-rest (car break-points))
                (map
                  (curryr - (car break-points))
                  (cdr break-points)))))) ;; STX curryr
    (let ((break-points (filter (λ (x) (< 0 x (length lst))) break-points)))
      (cond
        ((null? break-points) (list lst))
        (else (break-seq-it (list) lst break-points)))))

(define (depth lst)
  (cond
    ((list? lst) (if (null? lst) 1 (inc (apply max (map depth lst)))))
    (else 0)))

(define (transpose llst)
  (cond
    ((null? (cdr (car llst))) (list (flatten llst)))
    (else (merge (list (flatten (map car llst))) (transpose (map cdr llst))))))

(define (map-cycled f . seqs)
  (let* ( (max-count (apply max (map length seqs)))
          (is (range 1 (+ 1 max-count))))
    (for/fold
      ((res '()))
      ((i is))
      (pushr res (apply f (map (λ (x) (nth-cycled x i)) seqs))))))

(define (cleanmap seqs)
  (filter-not false? seqs))

(define (soft-merge #:op (op +) . args)
  (cond
    ((ormap string? args) (apply string-append (map str args)))
    (else (apply op args))))

(define (replace-by-part lst part target)
  (cond
    ((not (list? lst)) lst)
    (else (map
            (λ (el) (replace-by-part el part target))
            (list-substitute lst part target)))))

(define (remove-by-part lst part)
  (define (remove-by-part-1 lst part)
            (cond
              ((not (list? lst)) lst)
              (else (map
                      (λ (el) (remove-by-part-1 el part))
                      (exclude-all lst part)))))
    (cond
      ((vector? part)
        (for/fold
          ((res lst))
          ((p part))
          (remove-by-part-1 res p)))
      (else (remove-by-part-1 lst part))))

(define (append-unique lst . seqs)
  (cond
    ((empty? seqs) lst)
    (else
      (apply
        (curry
          append-unique
          (for/fold
            ((res lst))
            ((s (car seqs)))
            (pushr-unique res s)))
        (cdr seqs)))))

(define (by-index lst . args)
  (define (get-index arg)
    (match arg
      ((list initial-value f) (list initial-value f))
      (initial-value (list initial-value inc))
      (else (list 0 inc))))
  (let* ((indexes (map get-index args))
        (initial-values (map first indexes))
        (incrementors (map second indexes))
        )
    (for/fold
      ((res `((,(car lst) ,@initial-values))))
      ((element (cdr lst)))
      (let ((last-indexes-values (rest (last res))))
        (pushr res
          `(,element ,@(map (λ (x y) (x y)) incrementors last-indexes-values)))))))

(define (first-or-false lst)
  (and lst (not-empty? lst) (first-lst lst)))

(define (last? el lst)
  (equal? el (last lst)))

(define (several? lst)
  (and lst (list? lst) (> (length lst) 1)))

(define (partition-by-shift lst (step 1))
  (match lst
    ((list a b _ ...) (cond
                    ((< step 1) (error (format "step must be greater than 1, but actually is ~a" step)))
                    ((> step (length lst)) (list (list a b)))
                    (else
                      (pushl (partition-by-shift (drop lst step) step) (list a b)))))
    ((list a b) (list (list a b)))
    ((list a) empty)
    (else lst)))

(define (only-or-first x)
  (cond
    ((list? x)
      (if (not-empty? x)
        (first x)
        #f))
    (else x)))
