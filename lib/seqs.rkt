#lang racket

(provide (except-out (all-defined-out) c2))

(require compatibility/defmacro)
(require "base.rkt")
(require "controls.rkt")
(require "interval.rkt")
(require "debug.rkt")
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

(define (len seq)
  (if (string? seq)
    (string-length seq)
    (length seq)))

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

(define (implode lst (sep ""))
  (let ([seq (map tostring (clean nil? lst))]
        [sep (tostring sep)])
    (cond
      ((nil? seq) "")
      ((empty? (cdr seq)) (car seq))
      (else
        (string-append (car seq) sep (implode (cdr seq) sep))))))

(define (interleave sep seq)
  (cond
    ((string? seq) (implode (explode seq) sep))
    ((empty? (cdr seq)) seq)
    (else (push (car seq) sep (interleave sep (cdr seq))))))

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

(define (indexof seq el)
  ;(define (indexof-iter seq el acc)
  ;  (cond
  ;    ((null? seq) 0)
  ;    ((equal? el (car seq)) acc)
  ;    (else (indexof-iter (cdr seq) el (+ 1 acc)))))
  (if (string? seq)
    ;(indexof-iter (explode seq) el 1)
    (indexof (explode seq) el)
    ;(indexof-iter seq el 1)))
    (let ((res (member el seq)))
      (if res
        (+ 1 (- (length seq) (length (member el seq))))
        0))))


(define (indexof? seq el)
  (if (= 0 (indexof seq el)) #f #t))

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

(define (lpush seq el)
  (cons el seq))

(define pushl lpush)

(define (lpush-unique seq el)
  (if (indexof? seq el)
    seq
    (lpush seq el)))

(define (rshift seq (count 1))
  (reverse (lshift (reverse seq) count)))

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

(define (rpush seq el)
  (reverse (cons el (reverse seq))))

(define pushr rpush)

(define (rpush-unique seq el)
  (if (indexof? seq el)
    seq
    (rpush seq el)))

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


(define (exclude seq el)
  (if (string? seq)
    (implode (exclude (explode seq) el))
    (remove seq (indexof seq el))))

(define (exclude-all seq el)
  (let ((index (indexof seq el)))
    (cond
      ((> index 0) (exclude-all (exclude seq el) el))
      (else seq))))

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

(define (minus seq1 seq2)
  ;(clean
  ;  (λ (x)  (indexof? seq2 x))
  ;  seq1))
  (reverse (set-subtract seq1 seq2)))

;(define (minus/hash seq1 seq2)

(define (difference seq1 seq2)
  (merge
    (clean
      (λ (x) (indexof? seq2 x))
      seq1)
    (clean
      (λ (x) (indexof? seq1 x))
      seq2)))

(define (intersect seq1 seq2)
  ;(filter
  ;  (λ (x) (indexof? seq2 x))
  ;  seq1))
  (reverse (set-intersect seq1 seq2)))
