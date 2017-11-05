#lang racket

(require "../lib/all.rkt")
(require racket/stxparam)
(require (for-syntax racket/syntax "../lib/seqs.rkt" "../lib/controls.rkt"))

(provide (all-defined-out))

(define-syntax-parameter ER
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use ER inside sbgn:er")))

(define-syntax-parameter NAMES
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use NAMES inside sbgn:er")))

(define-syntax-rule (sbgn:er body ...)
  (let ((__er (hash))
        (__names (list)))
    (syntax-parameterize ((ER (make-rename-transformer #'__er))
                          (NAMES (make-rename-transformer #'__names)))
          ((λ () body ...)))))

; print environment hash
(define-syntax (env stx)
  (syntax-case stx ()
    ((_)
      #'ER)
    ((_ object-name)
      #'(hash-ref ER 'object-name))))

(define-syntax-rule (node name kind parameters ...)
  (set!
    ER
    (hash-insert
      ER
      (cons
        name
        (hash-insert
          (if (> (length '(parameters ...)) 0)
            (apply hash '(parameters ...))
            (hash))
          (cons ':_kind kind))))))

(define-syntax-rule (add-link kind from to name)
  (set!
    ER
    (hash-insert
      ER
      (cons
        name
        (hash ':from from ':to to ':_kind kind)))))

(define-syntax (link stx)
  (syntax-case stx ()
    ((_ kind (from to))
      (with-syntax ((generated-name #'(new-name)))
        #'(add-link 'kind 'from 'to generated-name)))
    ((_ kind name (from to)) #'(add-link 'kind 'from 'to 'name))))

(define (new-name-recur h (n (string->symbol (random-word 7 #:prefix "_"))))
  (if (indexof? h n)
    (new-name-recur h (string->symbol (random-word 7 #:prefix "_")))
    n))

(define-syntax (new-name stx)
  (syntax-case stx ()
    ((_)
        #'(let ((n (new-name-recur NAMES)))
            (set! NAMES (pushr NAMES n))
            n))))

;; diagram elements
(define-syntax-rule (entity name parameters ...)
  (node 'name 'entity parameters ...))

(define-syntax-rule (interaction args ...)
  (link 'interaction args ...))

(define-syntax-rule (modulation args ...)
  (link 'modulation args ...))

(define-syntax-rule (stimulation args ...)
  (link 'stimulation args ...))

(define-syntax-rule (necessary-stimulation args ...)
  (link 'necessary-stimulation args ...))

(define-syntax-rule (absolute-stimulation args ...)
  (link 'absolute-stimulation args ...))

(define-syntax-rule (inhibition args ...)
  (link 'inhibition args ...))

(define-syntax-rule (absolute-inhibition args ...)
  (link 'absolute-inhibition args ...))

(define-syntax-rule (assignment args ...)
  (link 'assignment args ...))
