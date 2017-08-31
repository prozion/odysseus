#lang racket

(require "../lib/all.rkt")

(provide (all-defined-out))

(define (csv->list-rows csv-str #:delimeter (delimeter ","))
  (let ((csv-str (opt/exclude-all csv-str "\r")))
    (map
      (λ (x) (opt/split x delimeter))
      (opt/split csv-str "\n"))))

(define (csv-file->list-rows filename #:delimeter (delimeter ","))
  (csv->list-rows (read-file filename) #:delimeter delimeter))

(define (csv-file->list-columns filename #:delimeter (delimeter ",") #:headers (headers #t))
  (let ((first-break-list (csv-file->list-rows filename #:delimeter delimeter)))
    (if headers
      (merge  (list (first first-break-list))
              (transpose (rest first-break-list)))
      (push   empty
              (transpose first-break-list)))))

(define (csv->hash csv-str #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1) #:columns-exclude (columns-exclude null))
  (let* (
        (content (csv->list-rows csv-str #:delimeter delimeter))
        (h (if (not headers)
              (range 1 (inc (length (first content))))
              (first content)))
        (content (if (not headers) content (rest content))))     
    (list->hash content h #:key-index key-index #:columns-exclude columns-exclude)))

(define (csv-file->hash filename #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1))
  (csv->hash (csv-file->list-rows filename)))

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
    (list->csv-file
      filename
      llst
      #:headers headers
      #:delimeter delimeter)))

(define (join-csv csv-file-1 csv-file-2 result-csv-file)
  #t)
