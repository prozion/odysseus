#lang racket

(provide (except-out (all-defined-out) c2))

(require compatibility/defmacro)


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

(define nil?
  (λ (v) (or (null? v) (and (string? v) (equal? v "")))))

(define (len seq)
  (if (string? seq)
    (string-length seq)
    (length seq)))

(define (join seq (delimeter ""))
  (c2
    (empty? (cdr seq))
      (car seq)
    (string-append (car seq) delimeter (join (cdr seq) delimeter))))

(define (split seq)
  (map string (string->list seq)))

(define (nth seq index)
  (cond
    ((string? seq) (if (nil? seq)
                      ""
                      (nth (split seq) index)))
    ((null? seq) null)
    ((< index 0) (nth seq (+ (length seq) index)))
    ((= index 0) (car seq))
    (else (nth (cdr seq) (sub1 index)))))

(define (triml seq count)
  (if (string? seq)
    (join (triml (split seq) count))
    (c2
      (= count 0)
        seq
      (triml (cdr seq) (sub1 count)))))

(define (trimr seq count)
  (if (string? seq)
    (join (trimr (split seq) count))
    (reverse (triml (reverse seq) count))))

(define (slice seq pos1 (pos2 (len seq)))
  (trimr
    (triml seq pos1)
    (- (len seq) pos2)))
