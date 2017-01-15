#lang racket

(require "seqs.rkt")
(require "alist.rkt")
(require "hash.rkt")

(require json)

(provide (all-defined-out))

(define (alist->json alst (first-time #t))
  (str (if first-time "{" "")
    (cond
      ;; if empty alist:
      ((null? alst) "")
      ;; if value of pair is a nested object:
      ((alist? (cadar alst)) (str
                                (format "\"~a\": \"~a\"~a"
                                        (caar alst)
                                        (alist->json (cadar alst) #t)
                                        ;; don't put comma, if nested object is in the last pair:
                                        (if (null? (cdr alst)) "" ", "))
                                (alist->json (cdr alst) #f)))
      ;; if last pair:
      ((null? (cdr alst)) (format "\"~a\": \"~a\"" (caar alst) (cadar alst)))
      (else (str
              (format "\"~a\": \"~a\", " (caar alst) (cadar alst))
              (alist->json (cdr alst) #f))))
    (if first-time "}" "")))

(define (json->alist jsonstr)
  (hash->list
    (string->jsexpr jsonstr)))
