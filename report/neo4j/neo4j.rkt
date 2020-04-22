#lang racket

(require "../../lib/_all.rkt")
(require "../../../settings/APIs.rkt")

(provide (all-defined-out))

(define (neo4jy k)
  (string-replace (->string k) "-" "_"))

(define (neo4jy-attr k)
  (->symbol (neo4jy k)))

(define (neo4jy-keys element)
  (hash-map (λ (k v)
              (values
                (neo4jy-attr k)
                v))
            element))

(define (cypher->json cypher-str (params-str #f))
  (format "{\"query\": \"~a\" ~a}"
          (string-replace cypher-str "\"" "")
          (if params-str
            (format ", \"params\": \"~a\"" params-str)
            "")))

(define (neo4j/authenticate (ip neo4j/ip))
  (let ((gdb-url (format "http://~a:7474/user/neo4j" ip)))
    (get-url
      gdb-url
      (list
        (format "Authorization: Basic ~a"
                          (string->base64 (format "~a:~a" neo4j/user neo4j/pwd)))
        "Accept: application/json; charset=UTF-8"))))

(define (cypher-request cyreq (ip neo4j/ip))
  (let ((gdb-url (format "http://~a:7474/db/data/cypher" ip)))
    (post-url
      gdb-url
      ;test/url
      (list
        "Accept: application/json; charset=UTF-8"
        "Content-Type: application/json"
        (format "Authorization: Basic ~a"
                          (string->base64 (format "~a:~a" neo4j/user neo4j/pwd))))
      (cypher->json cyreq))))

(define (neo4j/cypher cyreq)
  (json->hash
    (bytes->string/utf-8
      (cypher-request cyreq))))

;;;;;;;;;;;;

;(define (gdb/create-database db-name)
;  (neo4j/cypher (format "CREATE (n:~a)" db-name)))

; (gdb/create-node (hash :label "City" name "Moscow" country "Russia"))
(define-catch (gdb/create-node n #:label-attr (label-attr '_label) #:ignore-attrs (ignore-attrs empty))
  (let* ((label (hash-ref n label-attr))
        (label (if label (neo4jy label) label))
        (n-only-properties (hash-filter (λ (k v) (not (indexof? ignore-attrs k))) n))
        (n-only-properties (hash-map (λ (k v) (values k (->string v))) n-only-properties))
        (command (format
                  "CREATE (~a ~a)"
                  (if label (str ":" label) "")
                  (hash-print-json n-only-properties))))
      command))
    ; (neo4j/cypher command)))

; n1: (hash ':label "person:queens_giant" 'id 5 'name "Richard Roe"))
; n2: (hash ':label "person:queens_giant" 'id 7 'name "John Doe"))
; rel: (hash ':label "test_rel" 'rate 10)
(define-catch (gdb/create-rel id1 id2 relation-type #:label1 (label1 #f) #:label2 (label2 #f))
  (let ((command
            (format
              "MATCH (n1~a), (n2~a) WHERE ~a AND ~a CREATE (n1)-[r~a]->(n2)"
              (if label1 label1 "")
              (if label2 label2 "")
              (format "n1.id=\"~a\"" id1)
              (format "n2.id=\"~a\"" id2)
              (if relation-type (str ":" relation-type) ""))))
    command))
    ; (neo4j/cypher command)))

(define (neo4j-command/clean-database #:label (label #f) #:limit (limit #f))
  (format
    "MATCH (n~a) ~a DETACH DELETE n"
    (if label (str ":" label) "")
    (if limit (format "WITH n LIMIT ~a" limit) "")))

(define (gdb/delete-nodes #:label (label #f) #:limit (limit #f))
  (neo4j/cypher neo4j-command/clean-database #:label label #:limit limit))

(define (gdb/create-index label property)
  (neo4j/cypher
    (format "CREATE INDEX ON :~a(~a)" label property)))

(define (gdb/drop-index label property)
  (neo4j/cypher
    (format "DROP INDEX ON :~a(~a)" label property)))

; (gdb/import-csv "http://data.rusvegia.com/odysseus/test.csv" #:headers '(name surname rate) #:label "Test")
(define (gdb/import-csv csvfile #:headers (headers #f) #:label (label "Node") #:large-dataset (large-dataset #f))
  (let ((req
          (format "~a LOAD CSV ~a FROM '~a' AS row CREATE (:~a {~a});"
                  (if large-dataset "USING PERIODIC COMMIT" "")
                  (if headers "WITH HEADERS" "")
                  csvfile
                  (if (list? label) (implode label ":") label)
                  (if headers
                        (implode
                          (map
                            (λ (h) (str h ": row." h))
                            headers)
                          ", ")
                        "")
       )))
    ;(println req)
    (neo4j/cypher req)))


;  new code

    (define-catch (clear-neo4j-db)
      (string-append
        (format "match (n) optional match (n)-[r]-() delete n,r")
        (format "match (n) detach delete n")))

    (define-catch (load-from-csv csvfile)
      (format
          #<<TEXT
            USING PERIODIC COMMIT 5000
            LOAD CSV FROM ~a AS line
            MATCH (a:Author { authorid: line[0] })
            MATCH (b:Author { authorid: line[1] })
            CREATE (a)-[r:co_author { collaborations: line[2] }]->(b)
TEXT
        csvfile))
