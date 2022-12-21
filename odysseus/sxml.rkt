#lang racket

(require "base.rkt")
(require "type.rkt")
(require "list.rkt")
(require "hash.rkt")
(require "debug.rkt")
(require "io.rkt")
(require sxml)
(require (prefix-in hash: racket/hash))

(provide (all-defined-out))

(define (sxml->hxml sxml)
  (cond
    ((scalar? sxml) sxml)
    ((and (list? sxml) (empty? sxml)) "")
    ((and (list? sxml) (one-element? sxml)) (first sxml))
    ((list? sxml)
      (hash
        (first sxml)
        (let ((res-list (map sxml->hxml (rest sxml))))
          (cond
            ((one-element? res-list)
              (first res-list))
            ((equal*? (first sxml) '@)
                ; (apply hash:hash-union res-list))
                (apply hash-union res-list))
            (else
              res-list)))))))

(define (sxml->hhxml sxml)
  #t)

(define-catch (html->sxml html (tag "html"))
  (and (regexp-match? (pregexp (format "<~a.*?>.*?</~a>" tag tag)) html)
    (let* ((html-part (regexp-match (pregexp (format "<~a.*?>.*?</~a>" tag tag)) html))
          (html-part (and (list? html-part) (first html-part)))
          (sxml (and html-part (ssax:xml->sxml (open-input-string html-part) empty))))
      sxml
      )))
