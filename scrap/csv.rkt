#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

(define (csv-file->list filename #:delimeter (delimeter ",") #:headers (headers #t))
  (let
      ((first-break-list
        (map
          (λ (x) (split x delimeter))
          (split (read-file filename) "\n"))))
    (if headers
      (merge  (list (first first-break-list))
              (transpose (rest first-break-list)))
      (push  empty
              (transpose first-break-list)))))

(define (list->csv-file filename lst #:delimeter (delimeter ",") #:headers (headers #t) #:quoted (quoted #t))
  (write-file
    filename
    (implode
      (map
        (λ (s)
          (implode
            (if quoted
              (map (λ (ss) (str "\"" ss "\"")) s)
              s)
            delimeter))
        lst)
      "\n"))) ; STX curry, curryr

(define (hash->csv-file filename h #:headers (headers #f) #:delimeter (delimeter ","))
  (let ((headers (if headers headers (hash-keys (car (hash-values h))))))
    (println
      (pushl
        (hash-values
          (map-hash
            (λ (k v) (values k (hash->ordered-list v headers)))
            h))
        headers))
    (list->csv-file
      filename
      (pushl
        (hash-values
          (map-hash
            (λ (k v) (values k (hash->ordered-list v headers)))
            h))
        headers)
      #:headers headers
      #:delimeter delimeter)))
