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
  (let* ((content
            (opt/implode
              (map
                (λ (s)
                  (opt/implode
                    (if quoted
                      (map (λ (ss) (str "\"" (str/escape ss) "\"")) s)
                      s)
                    delimeter))
                lst)
              "\n")))
  (write-file filename content)))

(define (hash->csv-file filename h #:headers (headers #f) #:delimeter (delimeter ","))
  (let* ((headers (if headers headers (hash-keys (car (hash-values h)))))
        (llst (pushl
                (hash-values
                  (map-hash
                    (λ (k v) (values k (hash->ordered-list v headers)))
                    h))
                headers)))
    (println "hash->csv-file 1")
    (list->csv-file
      filename
      llst
      #:headers headers
      #:delimeter delimeter)))
