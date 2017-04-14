#lang racket

(require "../../lib/all.rkt")
(require "../../../settings/APIs.rkt")

(provide neo4j/authenticate neo4j/cypher gdb/create-database gdb/create-node gdb/import-csv)

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

(define (gdb/create-database db-name)
  (neo4j/cypher (format "CREATE (n:~a)" db-name)))

(define (gdb/create-node
            #:labels (labels #f)
            #:properties (properties #f))
  (neo4j/cypher
    (format
      "CREATE (n~a ~a)"
      (cond
        ((not labels) "")
        ((list? labels) (str ":" (implode labels ":")))
        (else (str ":" labels)))
      (cond
        ((not properties) "")
        ((hash? properties) (hash->json properties "'"))
        (else properties)))))

; (gdb/import-csv "http://data.rusvegia.com/odysseus/test.csv" #:headers '(name surname rate) #:labels "Test")
(define (gdb/import-csv csvfile #:headers (headers #f) #:labels (labels "Node"))
  (let ((req
          (format "LOAD CSV ~a FROM '~a' AS row CREATE (:~a {~a} );"
                  (if headers "WITH HEADERS" "")
                  csvfile
                  (if (list? labels) (implode labels ":") labels)
                  (if headers
                        (implode
                          (map
                            (λ (h) (str h ": row." h))
                            headers)
                          ", ")
                        "")
          )))
    (println req)
    (neo4j/cypher req)))
