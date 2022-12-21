#lang racket

(require "../json.rkt")

(require rackunit)

(define a '((a 10) (aa 20)))
(define b '((b 10) (bb 20)))

(check-equal? (alist->json '((a 10) (b 20))) "{\"a\":\"10\", \"b\":\"20\"}")
(check-equal? (alist->json `((a ,a) (b ,b))) "{\"a\":{\"a\":\"10\", \"aa\":\"20\"}, \"b\":{\"b\":\"10\", \"bb\":\"20\"}}")

(check-equal? (hash->json (hash 'a 10 'b (list (hash 'aa 20 'bb 30) (hash 'c 7 'e 8)) 'c (hash 'sp 300))) "{\"a\":\"10\", \"b\":[{\"aa\":\"20\", \"bb\":\"30\"}, {\"c\":\"7\", \"e\":\"8\"}], \"c\":{\"sp\":\"300\"}}")
