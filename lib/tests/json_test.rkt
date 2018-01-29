#lang racket

(module+ test

  (require rackunit)
  (require "../json.rkt")

  (define a '((a 10) (aa 20)))
  (define b '((b 10) (bb 20)))

  (check-equal? (alist->json '((a 10) (b 20))) "{\"a\":\"10\", \"b\":\"20\"}")
  (check-equal? (alist->json `((a ,a) (b ,b))) "{\"a\":{\"a\":\"10\", \"aa\":\"20\"}, \"b\":{\"b\":\"10\", \"bb\":\"20\"}}")
)
