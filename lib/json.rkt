#lang racket

(require "base.rkt")
(require "seqs.rkt")
(require "alist.rkt")
(require "type.rkt")
(require "hash.rkt")
(require "regexp.rkt")
(require "io.rkt")
(require "strings.rkt")

(require json)

(provide (all-defined-out))

(define-catch (alist->json alst (first-time #t))
  (str (if first-time "{" "")
    (cond
      ;; if empty alist:
      ((null? alst) "")
      ;; if value of pair is a nested object:
      ((alist? (cadar alst)) (str
                                (format "\"~a\":~a~a"
                                        (caar alst)
                                        (alist->json (cadar alst) #t)
                                        ;; don't put comma, if nested object is in the last pair:
                                        (if (null? (cdr alst)) "" ", "))
                                (alist->json (cdr alst) #f)))
      ;; if last pair:
      ((null? (cdr alst)) (format "\"~a\":\"~a\"" (caar alst) (cadar alst)))
      (else (str
              (format "\"~a\":\"~a\", " (caar alst) (cadar alst))
              (alist->json (cdr alst) #f))))
    (if first-time "}" "")))

(define-catch (json->alist jsonstr)
  (cond
    ((nil? jsonstr) (list empty))
    (else
      (hash->list
        (string->jsexpr jsonstr)))))

(define (json->hash jsonstr)
  (cond
    ((nil? jsonstr) (hash))
    (else
        (string->jsexpr jsonstr))))

(define-catch (hash->json h (q "\""))
  ; (alist->json
  ;   (hash->alist
  ;     (hash-map
  ;       (λ (k v) (values k (if q (str q v q) v)))
  ;       h))))
  (cond
    ((scalar? h) (if q (str q h q) h))
    ((list? h) (format "[~a]"
                        (implode
                            (map
                                (λ (e) (hash->json e q))
                                h)
                            ", ")))
    ((hash? h) (format "{~a}"
                      (implode
                            (map
                                (λ (k) (format "~a:~a"
                                          (if q (str q k q) k)
                                          (hash->json (hash-ref h k) q)))
                                (hash-keys h))
                            ", ")))
    (else "")))

(define (make-json-string val)
  (string->bytes/utf-8
    (format "~a"
      (cond
        ((not val) "Invalid format")
        (else (alist->json val))))))

(define (json-string json)
  (define res
  ((change-text
    (list
      (cons "\t" " ")
      (cons "  " " ")
      (cons "\r" "")
      (cons "\n" "\\n")
      ))
    json))
    ; (--- (map (λ (x) (cons x (char->integer x))) (string->list res)))
    res)


(module+ test

  (require rackunit)

  (define a '((a 10) (aa 20)))
  (define b '((b 10) (bb 20)))

  (check-equal? (alist->json '((a 10) (b 20))) "{\"a\":\"10\", \"b\":\"20\"}")
  (check-equal? (alist->json `((a ,a) (b ,b))) "{\"a\":{\"a\":\"10\", \"aa\":\"20\"}, \"b\":{\"b\":\"10\", \"bb\":\"20\"}}")

  (check-equal? (hash->json (hash 'a 10 'b (list (hash 'aa 20 'bb 30) (hash 'c 7 'e 8)) 'c (hash 'sp 300))) "{\"a\":\"10\", \"c\":{\"sp\":\"300\"}, \"b\":[{\"bb\":\"30\", \"aa\":\"20\"}, {\"e\":\"8\", \"c\":\"7\"}]}")
)
