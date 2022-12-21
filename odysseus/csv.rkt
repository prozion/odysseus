#lang racket

(require "../main.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; read ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-catch (csv-file->hashtree filename #:delimeter (delimeter ",") #:headers (headers #t) #:key-index (key-index 1) #:root (root "root") #:id (id '__id))
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
                        (list '__id))))
          ; remove lines with empty ids
          (hashtree (filter
                      (λ (h) (and
                                (hash-ref h '__id #f)
                                (not-equal? (hash-ref h '__id) "")))
                      hashtree))
          ; make id look like id
          (hashtree (map
                      (λ (h) (hash-union (hash '__id (->symbol (hash-ref h '__id) #:transform-function idfy)) h))
                      hashtree))
          ; make true hashtree in its form
          (hashtree (for/hash
                      ((h hashtree))
                      (values h (hash))))
          ; append root over the resulted hashtree
          (hashtree (hash
                      (hash '__id root)
                      hashtree)))
    hashtree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; write ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text->csv-text
  (change-text
    (list
      (cons "#f" "")
      (cons "\\" "")
      ; (cons "\"" "'")
      (cons "\"" "\"\"")
    )))

(define-catch (transform-csv-fields csv-items transform-hash)
  (map
    (λ (item)
      (for/fold
        ((res item))
        (((k v) transform-hash))
        (cond
          ((hash-ref res (->symbol k) #f)
              (hash-union
                (hash (->symbol k) (v (hash-ref* res k) res))
                res))
          (else res))))
    csv-items))

; headers :: (List ColumnName)
; data :: (List (Hash ColumnName Value)) | (List (List Value)) | (Value . Value)
(define-catch (get-csv headers data #:delimeter (delimeter ","))
  (let* ( (res-header (string-join (map ->string headers) delimeter))
          (data (if (hash? data) (hash-values data) data))
          (res-body (string-join
                      (map
                        (λ (row)
                          (cond
                            ((hash? row)
                              (string-join (map ->string (hash-refs row headers "")) delimeter))
                            ((list? row)
                              (string-join (map ->string row) delimeter))
                            ((cons? row)
                              (format "~a~a~a" (car row) delimeter (cdr row)))
                            (else
                              row)))
                        data)
                      "\n")))
      (string-append res-header "\n" res-body)))

(define-catch (write-csv-file headers data filename #:delimeter (delimeter ","))
    (write-file filename (get-csv headers data #:delimeter delimeter)))
