#lang racket

(require "../../lib/all.rkt")
(require "../../../settings/APIs.rkt")

(provide
    neo4j/authenticate neo4j/cypher
    ;gdb/create-database
    gdb/delete-nodes
    gdb/create-node gdb/create-rel
    gdb/create-index gdb/drop-index
    gdb/import-csv)

(define (cypher->json cypher-str (params-str #f))
  (format "{\"query\": \"~a\" ~a}"
          (string-replace cypher-str "\"" "")
          (if params-str
            (format ", \"params\": \"~a\"" params-str)
            "")))

(define (neo4j/authenticate (ip vps/ip))
  (let ((gdb-url (format "http://~a:7474/user/neo4j" ip)))
    (get-url
      test/url
      (list
        (format "Authorization: Basic ~a"
                          (string->base64 (format "~a:~a" vps/neo4j/user vps/neo4j/pwd)))
        "Accept: application/json; charset=UTF-8"))))

(define (cypher-request cyreq (ip vps/ip))
  (let ((gdb-url (format "http://~a:7474/db/data/cypher" ip)))
    (post-url
      gdb-url
      ;test/url
      (list
        "Accept: application/json; charset=UTF-8"
        "Content-Type: application/json"
        (format "Authorization: Basic ~a"
                          (string->base64 (format "~a:~a" vps/neo4j/user vps/neo4j/pwd))))
      (cypher->json cyreq))))

(define (neo4j/cypher cyreq)
  (json->hash
    (bytes->string/utf-8
      (cypher-request cyreq))))

;;;;;;;;;;;;

;(define (gdb/create-database db-name)
;  (neo4j/cypher (format "CREATE (n:~a)" db-name)))

(define (gdb/create-node n)
  (let ((label (@. n.:label))
        (properties (hash-delete n ':label)))
    (neo4j/cypher
      (format
        "CREATE (~a ~a)"
        (if label (str ":" label) "")
        (hash-print-json properties)))))

; n1: (hash ':label "person:queens_giant" 'id 5 'name "Richard Roe"))
; n2: (hash ':label "person:queens_giant" 'id 7 'name "John Doe"))
; rel: (hash ':label "test_rel" 'rate 10)
(define (gdb/create-rel n1 n2 rel)
  (let ((label1 (@. n1.:label))
        (label2 (@. n2.:label))
        (labelr (@. rel.:label)))
  (neo4j/cypher
    (format
      "MATCH (n1~a), (n2~a) WHERE ~a AND ~a CREATE (n1)-[r~a ~a]->(n2)"
      (if label1 label1 "")
      (if label2 label2 "")
      (hash-print (hash-delete n1 ':label) #:delimeter " AND " #:prefix "n1.")
      (hash-print (hash-delete n2 ':label) #:delimeter " AND " #:prefix "n2.")
      (if labelr (str ":" labelr) "")
      (hash-print-json (hash-delete rel ':label))))))

(define (gdb/delete-nodes #:label (label #f) #:limit (limit #f))
  (neo4j/cypher
    (format
      "MATCH (n~a) ~a DETACH DELETE n"
      (if label (str ":" label) "")
      (if limit (format "WITH n LIMIT ~a" limit) ""))))

(define (gdb/create-index label property)
  (neo4j/cypher
    (format "CREATE INDEX ON :~a(~a)" label property)))

(define (gdb/drop-index label property)
  (neo4j/cypher
    (format "DROP INDEX ON :~a(~a)" label property)))

; (gdb/import-csv "http://data.rusvegia.com/odysseus/test.csv" #:headers '(name surname rate) #:labels "Test")
(define (gdb/import-csv csvfile #:headers (headers #f) #:label (label "Node") #:large-dataset (large-dataset #f))
  (let ((req
          (format "~a LOAD CSV ~a FROM '~a' AS row CREATE (:~a {~a} );"
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
