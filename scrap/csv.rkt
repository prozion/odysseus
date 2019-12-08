#lang racket

(require "../lib/_all.rkt")

(provide (all-defined-out))

(define-catch (csv->list-rows csv-str #:delimeter (delimeter ","))
  (let* (
        (csv-str (opt/exclude-all csv-str "\r")))
    (map
      (λ (x) (opt/split x delimeter))
      (opt/split csv-str "\n"))))

(define-catch (csv-file->list-rows filename #:delimeter (delimeter ","))
  (csv->list-rows (read-file filename) #:delimeter delimeter))

(define-catch (csv-file->list-columns filename #:delimeter (delimeter ",") #:headers (headers #t))
  (let ((first-break-list (csv-file->list-rows filename #:delimeter delimeter)))
    (if headers
      (merge  (list (first first-break-list))
              (transpose (rest first-break-list)))
      (push   empty
              (transpose first-break-list)))))

(define-catch (csv->hash csv-str #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1) #:columns-exclude (columns-exclude null))
  (let* (
        (content (csv->list-rows csv-str #:delimeter delimeter))
        (h (if (not headers)
              (range 1 (inc (length (first content))))
              (first content)))
        (content (if (not headers) content (rest content))))
    (list->hash content h #:key-index key-index #:columns-exclude columns-exclude)))

(define-catch (csv-file->hash filename #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1))
  (csv->hash (read-file filename) #:delimeter delimeter #:headers headers #:key-index key-index))

(define-catch (csv-file->hashtree filename #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1) #:root (root "root") #:id (id 'id))
  (let* (
          (content (csv-file->list-rows filename #:delimeter delimeter))
          (h (if (not headers)
                (range 1 (inc (length (first content))))
                (map ->symbol (first content))))
          (_ (when-not (indexof? h id) (errorf "'~a' is not a field in a given CSV file, failed to detect id field" id)))
          (content (if (not headers) content (rest content)))
          (content (filter-not empty? content))
          ; make list of hashes from list of list of values
          (hashtree (for/list
                      ((line content))
                      (hash-keys-substitute
                        (for/hash
                          ((k h) (v line))
                          (values k v))
                        (list id)
                        (list 'id))))
          ; remove lines with empty ids
          (hashtree (filter
                      (λ (h) (and
                                (hash-ref h 'id #f)
                                (not-equal? (hash-ref h 'id) "")))
                      hashtree))
          ; make id look like id
          (hashtree (map
                      (λ (h) (hash-union (hash 'id (->symbol (hash-ref h 'id) #:transform-function idfy)) h))
                      hashtree))
          ; make true hashtree in its form
          (hashtree (for/hash
                      ((h hashtree))
                      (values h (hash))))
          ; append root over the resulted hashtree
          (hashtree (hash
                      (hash 'id root)
                      hashtree)))
    hashtree))

(define-catch (list->csv-file filename lst #:delimeter (delimeter ",") #:headers (headers #t) #:quoted (quoted #t))
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

(define-catch (hash->csv-file filename h #:headers (headers #f) #:delimeter (delimeter ","))
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
