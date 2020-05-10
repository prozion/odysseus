#lang racket

(provide (except-out (all-defined-out) c2))

(require compatibility/defmacro)
(require "base.rkt")
(require "controls.rkt")
(require racket/set)

;; algebra of operations with strings and lists (arrays)

;; save existing functions that do some different things
;; note! for some reason it works in REPL but not in DrRacket
;(define str string)
;(define string string-append)
;(define list-length length)

(define-macro (c2 zero-condition zero-op else-op)
  `(cond
    (,zero-condition ,zero-op)
    (else ,else-op)))

(define len (lambda (seq)
  (if (string? seq)
    (string-length seq)
    (length seq))))

(define (tostring lst)
  (format "~a" lst))

(define (reverse seq)
  (define (reverse-iter seq tail)
    (cond
      ((null? seq) seq)
      ((null? (cdr seq)) (cons (car seq) tail))
      (else (reverse-iter (cdr seq) (cons (car seq) tail)))))
  (if (string? seq)
    (implode (reverse (explode seq)))
    (reverse-iter seq null)))

(define (str . body)
  (implode body))

(define (list-pretty-string seq)
  (for/fold
    ((res ""))
    ((s seq))
    (string-append res s "\n")))

(define (implode lst (sep ""))
  (let ([seq (map
                (λ (x) (if (nil? x) "" (tostring x)))
                lst)] ;(clean nil? lst))]
        [sep (tostring sep)])
    (cond
      ((nil? seq) "")
      ((empty? (cdr seq)) (car seq))
      (else
        (string-append (car seq) sep (implode (cdr seq) sep))))))

(define (intermix sep seq)
  (cond
    ((string? seq) (implode (explode seq) sep))
    ((empty? (cdr seq)) seq)
    (else (push (car seq) sep (intermix sep (cdr seq))))))

(define (interleave l1 l2)
  (define (interleave-iter l1 l2 lres)
    (cond
      ((or (null? l1) (null? l2)) lres)
      (else (interleave-iter (cdr l1) (cdr l2) (rpush (rpush lres (car l1)) (car l2))))))
  (interleave-iter l1 l2 empty))

(define (explode seq)
  (map string (string->list seq)))

(define (split seq (sep null))
  (cond
    ((string? seq) (if (null? sep)
                      (explode seq)
                      (map implode (split (explode seq) sep))))
    ((empty? seq) empty)
    (else
      (let ((seppos (indexof seq sep)))
        (if (= seppos 0)
          (list seq)
          (push
            (lshift seq (sub1 seppos))
            (split (ltrim seq seppos) sep)))))))

(define (split-by-lambda-general seq f #:inclusive? (inclusive? #f))
  (let-values
    (((a b c d)
      (for/fold
        ((res (list)) (head (list)) (borders (list)) (rest seq))
        ((el seq))
        (cond
          ((f el) (values (if (empty? borders) (pushr res head) res) (list) (pushr borders el) (cdr rest)))
          ((equal? (cdr rest) null)
            (values (pushr (if (and inclusive? (not (empty? borders))) (pushr res borders) res) (pushr head el)) null null null))
          (else
            (let ((res (if (and inclusive? (not (empty? borders))) (pushr res borders) res)))
              (values res (pushr head el) (list) (cdr rest))))))))
    a))

(define (split-by-lambda seq f)
  (split-by-lambda-general seq f))

(define (split-by-lambda* seq f)
  (split-by-lambda-general seq f #:inclusive? #t))

(define (nth seq index)
  (let ((ll (len seq)))
    (cond
      ((string? seq) (if (nil? seq)
                        ""
                        (nth (split seq) index)))
      ((null? seq) null)
      ((= index 0) null)
      ((not (in (- ll) ll index)) null)
      ((< index 0) (nth seq (+ ll index 1)))
      ((= index 1) (car seq))
      (else
        ;(nth (cdr seq) (sub1 index)))))) ; slow version
        (list-ref seq (sub1 index))))))

(define (nth-cycled seq index)
  (let ((remnd (remainder index (length seq))))
    (cond
      ((>= (length seq) index) (nth seq index))
      ((= remnd 0) (nth seq -1))
      (else
        (nth seq (remainder index (length seq)))))))

(define (indexof seq el (compare-f equal?))
  (cond
    ((string? seq)
      (indexof (explode seq) el compare-f))
    ((list? seq)
      (let ((indexes
              (for/fold
                      ((res empty))
                      ((e seq) (i (in-naturals)))
                      (if (compare-f e el)
                        (pushr res (+ 1 i))
                        res))))
        (if (empty? indexes) 0 (car indexes))))
    (else 0)))

(define (indexof? seq el (compare-f equal?))
  (if (= 0 (indexof seq el compare-f)) #f #t))

(define (indexof*? seq el)
  (indexof? seq el (λ (a b) (equal? (tostring a) (tostring b)))))

(define (indexof-all seq el)
  (define (indexof-all-iter seq el acclist el-passed)
    (let ((index (indexof seq el)))
      (cond
        ((= index 0) acclist)
        (else (indexof-all-iter
                (ltrim seq (+ 1 index))
                el
                (rpush acclist (+ index el-passed))
                (+ index el-passed 1))))))
  (indexof-all-iter seq el empty 0))

(define (count-element seq el)
  (length (filter (curry equal? el) seq)))

(define (regexp-indexof? seq regxp)
  (ormap
    (λ (x) (regexp-match? (pregexp regxp) x))
    seq))

(define (lshift seq (count 1))
  (cond
    ((nil? seq) seq)
    ((string? seq) (implode (lshift (explode seq) count)))
    ((< count 1) empty)
    ((= count 1) (list (car seq)))
    ((> count (length seq)) seq)
    (else
      ;(cons (car seq) (lshift (cdr seq) (- count 1))))))
      (take seq count))))

(define shiftl lshift)

(define (lpop seq)
  (if (string? seq)
    (lpop (explode seq))
    (car seq)))

(define first lpop)

(define (ltrim seq (count 1))
  (cond
    ((string? seq) (implode (ltrim (explode seq) count)))
    ((null? seq) null)
    ((>= count (len seq)) empty)
    ((< count 0) empty)
    ((= count 0) seq)
    (else
      ;(ltrim (cdr seq) (sub1 count)))))
      (list-tail seq count))))

(define triml ltrim)

(define (lpush seq . els)
  (cond
    ((empty? els) seq)
    (else
      (apply
        (curry
          lpush
          (cons (car els) seq))
        (cdr els)))))

(define pushl lpush)

(define (op-unique op)
  (λ (seq el)
    (if (indexof? seq el)
      seq
      (op seq el))))

(define lpush-unique (op-unique lpush))
(define pushl-unique (op-unique pushl))

(define (rshift seq (count 1))
  (reverse (lshift (reverse seq) count)))

(define shiftr rshift)

(define (rpop seq)
  (if (string? seq)
    (rpop (explode seq))
    (car (reverse seq))))

(define last rpop)

(define (rtrim seq (count 1))
  (if (string? seq)
    (implode (rtrim (split seq) count))
    (reverse (ltrim (reverse seq) count))))

(define trimr rtrim)

(define (rpush seq . els)
  (cond
    ((empty? els) seq)
    (else
      (apply
        (curry
          rpush
          (reverse (cons (car els) (reverse seq))))
        (cdr els)))))

(define pushr rpush)

(define rpush-unique (op-unique rpush))
(define pushr-unique (op-unique pushr))

;; slice inclusively: slice c f -> a b [c d e f] g
(define (slice seq pos1 (pos2 (len seq)))
  (let ((ll (len seq)))
    (cond
      ((nil? seq) seq)
      ((< pos1 0) (slice seq (+ ll pos1 1) pos2))
      ((< pos2 0) (slice seq pos1 (+ ll pos2 1)))
      ((> pos2 ll) (slice seq pos1))
      (else
        (rtrim
          (ltrim seq (sub1 pos1))
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

(define (splice seq subseq pos)
  (if (string? seq)
    (implode (splice (explode seq) (explode subseq) pos))
    (merge (lshift seq (sub1 pos)) subseq (ltrim seq (sub1 pos)))))

(define (remove seq pos1 (pos2 pos1) #:len (size 1))
  (let ((ll (len seq)))
    (cond
      ((nil? seq) seq)
      ;((!= pos1 pos2) (merge (rtrim seq (- ll pos2)) (ltrim seq pos1)))
      ((not (in (- ll) ll pos1)) seq)
      ((not (in (- ll) ll pos2)) seq)
      ((> size 1) (remove seq pos1 (+ pos1 (sub1 size))))
      ((< pos1 0) (remove
                    seq
                    (+ ll pos1 1)
                    (if (= pos2 pos1) (+ ll pos1 1) pos2)))
      ((< pos2 0) (remove seq pos1 (+ ll pos2 1)))
      (else
        (merge
          (lshift seq (sub1 pos1))
          (rshift seq (- ll pos2)))))))

(define (exclude seq el #:compare (compare equal?))
  (if (string? seq)
    (implode (exclude (explode seq) el #:compare compare))
    (remove seq (indexof seq el compare))))

(define (exclude-all seq el)
  (let ((index (indexof seq el)))
    (cond
      ((> index 0) (exclude-all (exclude seq el) el))
      (else seq))))

(define (exclude* seq . el)
  (cond
    ((empty? el) seq)
    (else
      (apply
        (curry exclude* (exclude seq (car el)))
        (cdr el)))))

(define (exclude-all* seq . el)
  (cond
    ((empty? el) seq)
    (else
      (apply
        (curry exclude-all* (exclude-all seq (car el)))
        (cdr el)))))

(define (insert seq index el)
  (let ((ll (len seq)))
    (cond
      ((= index 0) seq)
      ((string? seq)
        (implode (insert (explode seq) index el)))
      ((empty? seq) (list el))
      ((or (= index -1) (= index (add1 ll)))
        (rpush seq el))
      ((in (- ll) -1 index) (insert seq (+ ll index 2) el))
      ((< index 0) (insert seq 1 el))
      ((> index (add1 ll)) seq)
      (else
        (merge
          (lshift seq (sub1 index))
          (list el)
          (rshift seq (- ll (sub1 index))))))))

(define (setn seq index newel)
  (cond
    ((nil? seq) seq)
    ((> index (len seq)) seq)
    (else (insert (remove seq index) index newel))))

(define (setns seq indexes newel)
  (cond
    ((empty? indexes) seq)
    (else (setns
            (setn seq (car indexes) newel)
            (cdr indexes)
            newel))))

(define (replace seq oldel newel)
  (setn seq (indexof seq oldel) newel))

(define (replace-all seq oldel newel)
  (cond
    ((> (indexof seq oldel) 0) (replace-all (replace seq oldel newel) oldel newel))
    (else seq)))

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

; (define (uniques seq)
;   (define (uniques-iter seq acc)
;     (cond
;       ((nil? seq) acc)
;       (else (uniques-iter
;               (ltrim seq)
;               (rpush-unique acc (first seq))))))
;   (uniques-iter seq null))

(define (uniques seq)
  (cond
    ((list? seq) (remove-duplicates seq))
    ((string? seq) (uniques (explode seq)))
    (else seq)))

(define (not-uniques seq)
  (define (not-uniques-iter seq acc)
    (cond
      ((nil? seq) acc)
      (else (not-uniques-iter
              (ltrim seq)
              (if (znil? (indexof (ltrim seq) (first seq)))
                acc
                (rpush acc (first seq)))))))
  (not-uniques-iter seq null))

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
          ((seq (cdr seqs)))
          (reverse (set-intersect res seq)))))))

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

(define (partition seq n)
  (define (partition-iter seq res)
    (cond
      ((< (length seq) n) res)
      ((< n 1) seq)
      (else
        (partition-iter
          (ltrim seq n)
          (pushr
            res
            (lshift seq n))))))
  (partition-iter seq empty))

(define (partition-all seq n)
  (define (partition-iter seq res)
    (cond
      ((< n 1) seq)
      ((empty? seq) res)
      ((< (length seq) n) (pushr res seq))
      (else
        (partition-iter
          (ltrim seq n)
          (pushr
            res
            (lshift seq n))))))
  (partition-iter seq empty))

  (define (break-seq seq break-points)
    (define (break-seq-it res-acc seq-rest break-points)
      (cond
        ((empty? break-points) (pushr res-acc seq-rest))
        (else (break-seq-it
                (pushr res-acc (lshift seq-rest (car break-points)))
                (ltrim seq-rest (car break-points))
                (map
                  (curryr - (car break-points))
                  (cdr break-points)))))) ;; STX curryr
    (let ((break-points (filter (λ (x) (< 0 x (length seq))) break-points)))
      (cond
        ((null? break-points) (list seq))
        (else (break-seq-it (list) seq break-points)))))

(define (depth seq)
  (cond
    ((list? seq) (if (null? seq) 1 (inc (apply max (map depth seq)))))
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

(define (append-unique seq . seqs)
  (cond
    ((empty? seqs) seq)
    (else
      (apply
        (curry
          append-unique
          (for/fold
            ((res seq))
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

(define (first-or-false seq)
  (and seq (not-empty? seq) (first seq)))

(module+ test

  (require rackunit)

  (check-equal? (len "") 0)
  (check-equal? (len "Oslo is a capital of Great Britain") 34)
  (check-equal? (len '()) 0)
  (check-equal? (len '(1 2 3 4 5)) 5)

  (check-equal? (tostring '(1 2 3)) "(1 2 3)")

  (check-equal? (reverse "Lestrigons") "snogirtseL")
  (check-equal? (reverse '(1 2 3 4 5)) '(5 4 3 2 1))

  (check-equal? (str "" "hello" " world!" "") "hello world!")
  (check-equal? (str "1" (unless #t "2") "3") "13")
  (check-equal? (str 123) "123")

  (check-equal? (implode '("a" "b" "c" "d") "-") "a-b-c-d")
  (check-equal? (implode '("long" "sail" "across" "the" "sea") " ") "long sail across the sea")

  (check-equal? (implode empty) "")
  (check-equal? (implode empty "-") "")
  (check-equal? (implode '(" " "b" "a" "k" "e" "\n" "r" "y")) " bake\nry")
  (check-equal? (implode '(1 2 3 4)) "1234")
  (check-equal? (implode '(1 2 3 4) "+") "1+2+3+4")
  (check-equal? (implode '(1) "+") "1")
  (check-equal? (implode (list null null null) ",") ",,")

  (check-equal? (intermix 0 '(1 2 3 4)) '(1 0 2 0 3 0 4))
  (check-equal? (intermix "." "Orest") "O.r.e.s.t")

  (check-equal? (interleave '(1 2) '(3 4)) '(1 3 2 4))
  (check-equal? (interleave '(1 2 10) '(3 4)) '(1 3 2 4))
  (check-equal? (interleave '(1 2) '(3 4 10)) '(1 3 2 4))

  (check-equal? (explode "") empty)
  (check-equal? (explode " bake\nry") '(" " "b" "a" "k" "e" "\n" "r" "y"))

  (check-equal? (split "") '())
  (check-equal? (split "Oslo") '("O" "s" "l" "o"))
  (check-equal? (split "a") '("a"))
  (check-equal? (split "Dar-as-Salam" "-") '("Dar" "as" "Salam"))
  ; (check-equal? (split "Dar, as, Salam" ", ") '("Dar" "as" "Salam")) ; use string-split in these cases
  (check-equal? (split '(1 2 0 3 4 0 5 0 6) 0) '((1 2) (3 4) (5) (6)))

  (check-equal? (split-by-lambda '(1 2 3 4 5 6 7) (λ (x) (equal? x 4))) '((1 2 3) (5 6 7)))
  (check-equal? (split-by-lambda* '(1 2 3 4 5 6 7) (λ (x) (equal? x 4))) '((1 2 3) (4) (5 6 7)))
  (check-equal? (split-by-lambda '(1 2 3 11 4 5 13 6 7) (λ (x) (> x 10))) '((1 2 3) (4 5) (6 7)))
  (check-equal? (split-by-lambda* '(1 2 3 11 4 5 13 6 7) (λ (x) (> x 10))) '((1 2 3) (11) (4 5) (13) (6 7)))
  (check-equal? (split-by-lambda '(1 2 3 11 12 13 4 5 13 6 18 7) (λ (x) (> x 10))) '((1 2 3) (4 5) (6) (7)))
  (check-equal? (split-by-lambda* '(1 2 3 11 12 13 4 5 13 6 18 7) (λ (x) (> x 10))) '((1 2 3) (11 12 13) (4 5) (13) (6) (18) (7)))

  (check-equal? (nth "" 10) "")
  (check-equal? (nth "Oslo god morgen" 0) null)
  (check-equal? (nth "Oslo god morgen" 1) "O")
  (check-equal? (nth "Oslo god morgen" 6) "g")
  (check-equal? (nth "ost og skinke" -1) "e")
  (check-equal? (nth '() 10) null)
  (check-equal? (nth '(0 1 2 3 4 5) 0) null)
  (check-equal? (nth '(0 1 2 3 4 5) 1) 0)
  (check-equal? (nth '(0 1 2 3 4 5) 3) 2)
  (check-equal? (nth '(0 1 2 3 4 5) -1) 5)
  (check-equal? (nth '(0 1 2 3 4 5) 7) null)

  (check-equal? (nth-cycled '(0 1 2 3 4 5) 7) 0)
  (check-equal? (nth-cycled '(0 1 2 3 4 5) 17) 4)
  (check-equal? (nth-cycled '(1 2 3 4 5) 10) 5)

  (check-equal? (indexof "" "e") 0)
  (check-equal? (indexof "Hercules" "e") 2)
  (check-equal? (indexof "Hercules" "a") 0)
  (check-equal? (indexof '(11 -22 30 80 -5) 30) 3)
  (check-equal? (indexof '(11 -22 30 80 -5) -5) 5)
  (check-equal? (indexof '(11 -22 30 80 -5) 333) 0)
  (check-equal? (indexof '(11 -22 30 80 -5) -5 (λ (x y) (equal? x 30))) 3)

  (check-false (indexof? 'a 'd))
  (check-true (indexof? '(a b c d e f) 'd))
  (check-true (indexof? '(1 (10 1) 2) '(10 1)))
  (check-false (indexof? '(1 2 3 4 5) 6))
  (check-true (indexof? "abcdef" "d"))
  (check-false (indexof? "abcdef" "k"))

  (check-equal? (indexof-all "Hercules" "e") '(2 7))
  (check-equal? (indexof-all '(11 8 -22  8 30 80 -5 8) 8) '(2 4 8))

  (check-equal? (count-element '(11 8 -22  8 30 80 -5 8) 8) 3)
  (check-equal? (count-element '(1 1 1) 1) 3)

  (check-true (regexp-indexof? '("doo" "fowl" "island") "doo"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "i.*d"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "do{2}"))
  (check-true (regexp-indexof? '("doo" "fowl" "island") "f[oae]wl"))
  (check-false (regexp-indexof? '("doo" "fowl" "island") "baz"))
  (check-false (regexp-indexof? '("doo" "fowl" "island") "f[auy]+wl"))

  (check-equal? (lshift "" 10) "")
  (check-equal? (lshift "Black waters" 5) "Black")
  (check-equal? (lshift "Black waters" 0) "")
  (check-equal? (lshift "Black waters" 100) "Black waters")
  (check-equal? (lshift '(1 2 3 4 5 6 7 8 9)) '(1))
  (check-equal? (lshift '(1 2 3 4 5 6 7 8 9) 2) '(1 2))

  (check-equal? (lpop '(1 2 3 4 5 6 7 8 9)) 1)
  (check-equal? (first "Andromachus") "A")

  (check-equal? (ltrim "" 10) "")
  (check-equal? (ltrim "Oslo god morgen" 0) "Oslo god morgen")
  (check-equal? (ltrim "Oslo god morgen") "slo god morgen")
  (check-equal? (ltrim "Oslo god morgen" 10) "orgen")
  (check-equal? (ltrim "Oslo god morgen" 100) "")
  (check-equal? (ltrim "Oslo god morgen" -5) "") ; add contract!
  (check-equal? (ltrim '(1 2 3 4 5 6 7 8 9) 2) '(3 4 5 6 7 8 9))

  (check-equal? (lpush '() 100) '(100))
  (check-equal? (lpush '(1 2 3) 100) '(100 1 2 3))
  (check-equal? (lpush '((1 2 3)) '(100 200)) '((100 200) (1 2 3)))
  (check-equal? (lpush '(1 2) 3 4 5) '(5 4 3 1 2))
  (check-equal? (pushl '(1 2) 3 4 5) '(5 4 3 1 2))

  (check-equal? (lpush-unique '(1 2 3) 100) '(100 1 2 3))
  (check-equal? (lpush-unique '(1 2 3) 3) '(1 2 3))

  (check-equal? (rshift "" 10) "")
  (check-equal? (rshift "Black waters" 3) "ers")
  (check-equal? (rshift "Black waters" 0) "")
  (check-equal? (rshift "Black waters" 100) "Black waters")
  (check-equal? (rshift '(1 2 3 4 5 6 7 8 9)) '(9))
  (check-equal? (rshift '(1 2 3 4 5 6 7 8 9) 2) '(8 9))

  (check-equal? (rpop '(1 2 3 4 5 6 7 8 9)) 9)
  (check-equal? (last "Andromachus") "s")

  (check-equal? (rtrim "" 10) "")
  (check-equal? (rtrim "Oslo god morgen" 0) "Oslo god morgen")
  (check-equal? (rtrim "Oslo god morgen" 9) "Oslo g")
  (check-equal? (rtrim "Oslo god morgen" 100) "")
  (check-equal? (rtrim "Oslo god morgen" -5) "") ; add contract!
  (check-equal? (rtrim '(1 2 3 4 5 6 7 8 9) 2) '(1 2 3 4 5 6 7))
  (check-equal? (rtrim '(1 2 3 4 5 6 7 8 9) 0) '(1 2 3 4 5 6 7 8 9))

  (check-equal? (rpush '() 100) '(100))
  (check-equal? (rpush '(1 2 3) 100) '(1 2 3 100))
  (check-equal? (rpush '(1 2 3) 3) '(1 2 3 3))
  (check-equal? (pushr '(1 2 3) 3) '(1 2 3 3))
  (check-equal? (pushr '(1) 2 3 4) '(1 2 3 4))
  (check-equal? (pushr '(1) 2 '(3 4)) '(1 2 (3 4)))

  (check-equal? (rpush-unique '(1 2 3) 100) '(1 2 3 100))
  (check-equal? (rpush-unique '(1 2 3) 3) '(1 2 3))

  (check-equal? (slice "" 1 3) "")
  (check-equal? (slice "Oslo god morgen" 3 1) "")
  (check-equal? (slice "Oslo god morgen" 2 7) "slo go")
  (check-equal? (slice "Oslo god morgen" 4) "o god morgen")
  (check-equal? (slice "Oslo god morgen" 4 100) "o god morgen")
  (check-equal? (slice "Oslo god morgen" 4 -3) "o god morg")
  (check-equal? (slice '() 1 3) null)
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 7) '(2 3 4 5 6 7))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 2 100) '(2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 100) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 3) '(1 2 3))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -1) '(1 2 3 4 5 6 7 8 9))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 1 -3) '(1 2 3 4 5 6 7))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) -4 -2) '(6 7 8))
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 2) '())
  (check-equal? (slice '(1 2 3 4 5 6 7 8 9) 3 3) '(3))

  (check-equal? (merge "hello" " " "world" "!") "hello world!")
  (check-equal? (merge '(1 2 3 4) '(100 200) '("a" "b")) '(1 2 3 4 100 200 "a" "b"))

  (check-equal? (merge-unique '(1 2 3 4) '(100 200)) '(1 2 3 4 100 200))
  (check-equal? (merge-unique '(1 2 3 4) '(5 1 8 2 4)) '(1 2 3 4 5 8))
  (check-equal? (merge-unique '(1 2 3 4) '(5 1 8 2 4) '(0 3 5 7)) '(1 2 3 4 5 8 0 7))
  (check-equal? (merge-unique '(1 2 3 4) 5) '(1 2 3 4))

  (check-equal? (push '(1 2 3 4) '(100 200) '("a" "b")) '((1 2 3 4) (100 200) "a" "b"))

  (check-equal? (concat '(1 2 3 4) '(100 200) '("a" "b")) '((1 2 3 4) (100 200) ("a" "b")))

  (check-equal? (splice
    "Tell me, of that ingenious hero" "O muse, " 10)
    "Tell me, O muse, of that ingenious hero")
  (check-equal? (splice '(1 2 3 4 5 6) '(100 200) 3) '(1 2 100 200 3 4 5 6))

  (check-equal? (remove "Agamemnon" 4) "Agaemnon")
  (check-equal? (remove "Agamemnon" 5 8) "Agamn")
  (check-equal? (remove "Agamemnon" 5 #:len 3) "Agamon")
  (check-equal? (remove "Clytemnestra" 4 -1) "Cly")
  (check-equal? (remove "Oslo" 4) "Osl")
  (check-equal? (remove "Agamemnon" -1) "Agamemno")
  (check-equal? (remove '(1 2 3 4 5 6 7) 4) '(1 2 3 5 6 7))
  (check-equal? (remove '(1) 1) '())
  (check-equal? (remove '(1 2 3 4 5 6 7) 4 5) '(1 2 3 6 7))
  (check-equal? (remove '(1 2 3 4 5 6 7) 4 #:len 3) '(1 2 3 7))

  (check-equal? (exclude '(1 2 3 4 5 6 7) 3) '(1 2 4 5 6 7))
  (check-equal? (exclude* '(1 2 3 4 5 6 7) 3 2 1) '(4 5 6 7))
  (check-equal? (exclude* '(1 2 3 4 5 6 7 3) 3 2 1) '(4 5 6 7 3))
  (check-equal? (exclude '(3) 3) '())
  (check-equal? (exclude '(1 2 3 4 5 6 7) 10) '(1 2 3 4 5 6 7))
  (check-equal? (exclude '(1 2 3 4 5 6 7) 5 #:compare (λ (x y) (= x 3))) '(1 2 4 5 6 7))
  (check-equal? (exclude '(1 2 "c" 4 5 "c" 7) "c") '(1 2 4 5 "c" 7))
  (check-equal? (exclude '(1 2 (5 8) 4 5 7) '(5 8)) '(1 2 4 5 7))
  (check-equal? (exclude "Tell me, O muse, of that ingenious hero" "o") "Tell me, O muse, f that ingenious hero")

  (check-equal? (exclude-all '(1 2 "c" 4 5 "c" 7) "c") '(1 2 4 5 7))
  (check-equal? (exclude-all '(1 1 1) 1) '())
  (check-equal? (exclude-all '(1 2 "c" 4 5 "c" 7) "d") '(1 2 "c" 4 5 "c" 7))
  (check-equal? (exclude-all '(1 2 (5 8) 4 5 (5 8) 7) '(5 8)) '(1 2 4 5 7))
  (check-equal? (exclude-all* '[1 2 (5 8) 4 5 (5 8) 4 7] '(5 8) 2 4) '(1 5 7))
  (check-equal? (exclude-all "Tell me, O muse, of that ingenious hero" "o") "Tell me, O muse, f that ingenius her")
  (check-equal? (exclude-all "Tell me,\r\n O muse,\r of that ingenious hero\r" "\r") "Tell me,\n O muse, of that ingenious hero")

  (check-equal? (insert "" 0 "") "")
  (check-equal? (insert "" 0 "a") "")
  (check-equal? (insert "" 1 "a") "a")
  (check-equal? (insert "Itaka" 1 "_") "_Itaka")
  (check-equal? (insert "Itaka" 3 "h") "Ithaka")
  (check-equal? (insert "Itaka" 6 "s") "Itakas")
  (check-equal? (insert "Itaka" 7 "z") "Itaka")
  (check-equal? (insert "Itaka" -1 "Y") "ItakaY")
  (check-equal? (insert '(1 2 3 4 5) 3 100) '(1 2 100 3 4 5))

  (check-equal? (setn "" 0 "a") "")
  (check-equal? (setn "" -1 "a") "")
  (check-equal? (setn "" 5 "d") "")
  (check-equal? (setn "Troy" -1 "e") "Troe")
  (check-equal? (setn "Troy" 4 "U") "TroU")
  (check-equal? (setn "Troy" 5 "U") "Troy")
  (check-equal? (setn "Troy" 6 "U") "Troy")
  (check-equal? (setn "Troy" 1 "B") "Broy")
  (check-equal? (setn "Mycenae" -3 "R") "MyceRae")
  (check-equal? (setn "Sparta" 5 "c") "Sparca")
  (check-equal? (setn '(1 2 3 4 5 6 7) 6 100) '(1 2 3 4 5 100 7))

  (check-equal? (setns '(1 2 3 4 5 6 7) '(1 4 6) 100) '(100 2 3 100 5 100 7))
  (check-equal? (setns "Sparta" '(2 3 5) "c") "Sccrca")

  (check-equal? (replace "" "a" "o") "")
  (check-equal? (replace "Homer" "r" " ") "Home ")
  (check-equal? (replace "Sparta" "a" "o") "Sporta")
  (check-equal? (replace "Sparta" "b" "u") "Sparta")
  (check-equal? (replace "Sparta" "S" "This is S") "This is Sparta")
  (check-equal? (replace '(1 2 3 4 5 3) 3 30) '(1 2 30 4 5 3))

  (check-equal? (replace-all "Sparta" "a" "o") "Sporto")
  (check-equal? (replace-all "Tell me, O muse, of that ingenious hero" " " "_") "Tell_me,_O_muse,_of_that_ingenious_hero")
  (check-equal? (replace-all "Tell me, O muse, of that ingenious hero" "w" "_") "Tell me, O muse, of that ingenious hero")
  (check-equal? (replace-all '(1 2 3 4 5 3) 3 30) '(1 2 30 4 5 30))
  (check-equal? (replace-all '(1 2 3 4 5 3) 7 30) '(1 2 3 4 5 3))

  (check-equal? (list-substitute '(1 3 2 (3 4) 5 3) 3 30) '(1 30 2 (30 4) 5 30))
  (check-equal? (list-substitute '(1 3 2 (3 4) 5 3) 3 "hello") '(1 "hello" 2 ("hello" 4) 5 "hello"))
  (check-equal? (list-substitute '(1 2 (a 4 c (a ((a))) 5 a)) 'a 'b) '(1 2 (b 4 c (b ((b))) 5 b)))

  (check-equal? (not-uniques '(1 2 3 1 10 7 3 4 4)) '(1 3 4))
  (check-equal? (not-uniques "abcdefa") '("a"))
  (check-equal? (not-uniques '(1 2 13 12 10 7 3 4 14)) '())

  (check-equal? (uniques '(1 2 3 1 10 7 3 4 4)) '(1 2 3 10 7 4))
  (check-equal? (uniques '(1 2 (3 4) 1 10 7 3 4 4 (3 4))) '(1 2 (3 4) 10 7 3 4))

  (check-equal? (minus '() '()) '())
  (check-equal? (minus '(1 2 3) '()) '(1 2 3))
  (check-equal? (minus '() '(1 2 3)) '())
  (check-equal? (minus '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3))
  (check-equal? (minus '(1 2 3 4 5) '(2 1 3 5 4)) '())
  (check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4))) '((1 2)))
  (check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '())
  (check-equal? (minus '((1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal?) '((1 2)))
  (check-equal? (minus '((-1 0) (1 2) (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '((-1 0)))
  (check-equal? (minus '((-1 0) 3 (3 4)) '((2 1) (3 4)) #:equal-f equal-set?) '((-1 0) 3))

  (check-equal? (minus '((-1 0) 3 (3 4) ((10 20) (30 40))) '((2 1) (3 4) ((40 30) (20 10))) #:equal-f deep-equal-set?) '((-1 0) 3))

  (check-equal? (intersect '() '()) '())
  (check-equal? (intersect '(1 2 3) '()) '())
  (check-equal? (intersect '() '(1 2 3)) '())
  (check-equal? (intersect '(1 2 3 4 5) '(9 8 7 6 5 4)) '(4 5))
  (check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6)) '(1 2 3))
  (check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6) '(1 2 8)) '(1 2))
  (check-equal? (intersect '(1 2 3 5) '(3 2 1 1 6) '()) '())
  (check-equal? (intersect '(1 2 3 5) #f) '())

  (check-true (intersect? '(1 2 3 5) '(3 2 1 1 6)))
  (check-false (intersect? '(1 2 3 5) '(4 8 10)))
  (check-false (intersect? '(1 2 3 5) #f))

  (check-equal? (difference '() '()) '())
  (check-equal? (difference '(1 2 3) '()) '(1 2 3))
  (check-equal? (difference '() '(1 2 3)) '(1 2 3))
  (check-equal? (difference '(1 2 3 4 5) '(9 8 7 6 5 4)) '(1 2 3 9 8 7 6))
  (check-equal? (difference '(1 2 3 5) '(3 2 1 1 6)) '(5 6))
  (check-equal? (difference '(1 2 3 5) '(5 2 1 3)) '())
  (check-equal? (difference '(1 1 1 1) '()) '(1 1 1 1))

  (check-equal? (unique-difference '(1 1 1 1) '()) '(1))

  (check-true (equal-elements? '() '()))
  (check-equal? (equal-elements? '(#f) (list (< 3 2))) #t)
  (check-true (equal-elements? '(1 1 1 1) '(1)))
  (check-true (equal-elements? '(1 2 3) '(1 2 3)))
  (check-true (equal-elements? '(1 2 3) '(3 2 1 1)))
  (check-false (equal-elements? '(1 1 1 1) '(1 1 1 2)))
  (check-false (equal-elements? '(1 1 1 1) '(2 2 2)))
  (check-false (equal-elements? '(1 1 1 1) '()))

  (check-true (equal-set? '(1 2 3) '(1 2 3)))
  (check-true (equal-set? '(1 2 3) '(3 2 1)))
  (check-true (equal-set? '(1 1 2 3) '(3 1 2 1)))
  (check-true (equal-set? '() '()))
  (check-true (equal-set? '(2) (list (- 3 1))))
  (check-false (equal-set? '(1 2 3) '(3 2 1 1)))
  (check-false (equal-set? '(1 2 3 3) '(3 2 1 1)))
  (check-false (equal-set? '(1 2 3) '(1 2 3 4)))
  (check-false (equal-set? '(1 2 3) '()))
  (check-false (equal-set? '() '(1 2 3)))

  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(3 1 2)) '(9))
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(3 1 2 0)) '9)
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(2)) '(4 5))
  (check-equal? (nlist-ref '(1 (2 3) (4 5) (6 (7 8 (9))) 10) '(1 0)) 2)

  (check-equal? (partition '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
  (check-equal? (partition '(1 2 3 4 5 6 7 8 9) 4) '((1 2 3 4) (5 6 7 8)))
  (check-equal? (partition '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
  (check-equal? (partition '(1 2 3 4 5) 0) '(1 2 3 4 5))
  (check-equal? (partition `(key (,(hash 'a 10) ,(hash 'b 20))) 2) `((key (,(hash 'a 10) ,(hash 'b 20)))))

  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9) 4) '((1 2 3 4) (5 6 7 8) (9)))
  (check-equal? (partition-all '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12) (13 14 15)))
  (check-equal? (partition-all '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
  (check-equal? (partition-all '(1 2 3 4 5) 0) '(1 2 3 4 5))

  (check-equal? (break-seq '(1 2 3 4 5 6) '()) '((1 2 3 4 5 6)))
  (check-equal? (break-seq '(1 2 3 4 5 6) '(0)) '((1 2 3 4 5 6)))
  (check-equal? (break-seq '(1 2 3 4 5 6) '(4)) '((1 2 3 4) (5 6)))
  (check-equal? (break-seq '(1 2 3 4 5 6) '(6)) '((1 2 3 4 5 6)))
  (check-equal? (break-seq '(1 2 3 4 5 6) '(3 5)) '((1 2 3) (4 5) (6)))
  (check-equal? (break-seq '(1 2 3 4 5 6) '(0 3 5 6)) '((1 2 3) (4 5) (6)))

  (check-equal? (flatten '((1 2 3) (4 5 6))) '(1 2 3 4 5 6))
  (check-equal? (flatten '((1 (2 (3))) (4 ((5)) 6) 7)) '(1 2 3 4 5 6 7))
  (check-equal? (flatten '(1 2 3)) '(1 2 3))

  (check-equal? (depth 3) 0)
  (check-equal? (depth '(1 2 3)) 1)
  (check-equal? (depth '(())) 2)
  (check-equal? (depth '(1 2 (3 4) (5 6 (7) (8 9 (10))) (11 (12 (13 14 (15 16)) 17)))) 5)

  (check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))

  (check-equal? (map-cycled (λ (a b c) (+ b c)) '(1 2 3 4 5 6) '(10 20) '(5 15 25)) '(15 35 35 25 25 45))

  (check-equal? (cleanmap '(1 2 3 #f 4 #f "a" '() 3)) '(1 2 3 4 "a" '() 3))

  (check-equal? (soft-merge "c") "c")
  (check-equal? (soft-merge 567) 567)
  (check-equal? (soft-merge 1 2) 3)
  (check-equal? (soft-merge 1 2.5) 3.5)
  (check-equal? (soft-merge 1 2.5 #:op -) -1.5)
  (check-equal? (soft-merge #:op (λ args (/ (apply + args) (len args))) 1 3 4 5 7) 4)
  (check-equal? (soft-merge 1 "a") "1a")
  (check-equal? (soft-merge "bc" 2 48.5 "o") "bc248.5o")
  (check-equal? (soft-merge "c" "de") "cde")
  (check-equal? (soft-merge "moscow" " " "calling") "moscow calling")
  ;(check-equal? (soft-merge '(1 2) 5) '(1 2 5))
  ;(check-equal? (soft-merge "d" (hash 'a 10 'b 20)) "d")
  ;(check-equal? (soft-merge (hash 'a 10 'b 20) "d") (hash 'a 10 'b 20 "d" null))

  (check-equal? (replace-by-part '(1 2 3 4 5) 3 10) '(1 2 10 4 5))
  (check-equal? (replace-by-part '(1 2 3 4 5) 6 10) '(1 2 3 4 5))
  (check-equal? (replace-by-part '(1 2 (3 (8 9 10) 4) 5) 8 100) '(1 2 (3 (100 9 10) 4) 5))

  (check-equal? (remove-by-part '(1 2 3 4 5) 3) '(1 2 4 5))
  (check-equal? (remove-by-part '(1 2 3 4 3 5) 3) '(1 2 4 5))
  (check-equal? (remove-by-part '(1 2 3 (1 3 5) 4 3 5) 3) '(1 2 (1 5) 4 5))
  (check-equal? (remove-by-part '(1 2 3 4 5) 6) '(1 2 3 4 5))
  (check-equal? (remove-by-part '(1 2 3 4 5) '#(6 8 1 3)) '(2 4 5))
  (check-equal? (remove-by-part '((a) b (c (d)) (e)) '(c (d))) '((a) b (e)))
  (check-equal? (remove-by-part '((a) b (c (d)) (e)) '#((c (d)) (e))) '((a) b))
  (check-equal? (remove-by-part '((a) b (c (d)) (e)) '(d)) '((a) b (c) (e)))
  (check-equal? (remove-by-part '((a) b (c (d)) (b)) 'b) '((a) (c (d)) ()))

  (check-equal? (append-unique '(1 2 3) '(4 5 1 6)) '(1 2 3 4 5 6))
  (check-equal? (append-unique '(1 2 1 3) '(4 5 1 6 2 2 2)) '(1 2 1 3 4 5 6))
  (check-equal? (append-unique '(1 2 1 3) '(4 5 1 6 2) '(2 2) '(7 3 8 8 (9) 10)) '(1 2 1 3 4 5 6 7 8 (9) 10))

  (check-equal? (by-index '(0 1 2 3) 0) '((0 0) (1 1) (2 2) (3 3)))
  (check-equal? (by-index '(0 1 2 3) (list 0 inc)) '((0 0) (1 1) (2 2) (3 3)))
  (check-equal? (by-index '(0 1 2 3) (list -10 (λ (i) (* i -2)))) '((0 -10) (1 20) (2 -40) (3 80)))
)
