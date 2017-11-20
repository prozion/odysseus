#lang racket

(require "../lib/all.rkt")
(require racket/stxparam)
(require (for-syntax racket/syntax "../lib/seqs.rkt" "../lib/controls.rkt"))
(require "er-rules.rkt")

(provide (all-defined-out))

(define-syntax-parameter ER
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use ER inside 'sbgn:er'")))

(define-syntax-parameter NAMES
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use NAMES inside 'sbgn:er'")))

(define-syntax-parameter COUNT-STEPS
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use COUNT-STEPS  inside 'experiment'")))

(define-syntax-parameter PROCS
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use PROCS inside 'experiment'")))

(define-syntax-rule (sbgn:er body ...)
  (let ((__er (hash))
        (__names (list)))
    (syntax-parameterize ((ER (make-rename-transformer #'__er))
                          (NAMES (make-rename-transformer #'__names)))
          ((λ () body ...))
          ER)))

(define-syntax-rule (node name type parameters ...)
  (set!
    ER
    (hash-insert
      ER
      (cons
        name
        (hash-union
          (if (> (length '(parameters ...)) 0)
            (apply hash '(parameters ...))
            (hash))
          (hash '_type type 'state 'exists))))))

(define-syntax-rule (add-link type from to name)
  (let ((to-list (if (list? to) to (list to))))
    (set!
      ER
      (hash-insert
        ER
        (cons
          name
          (hash '_from from '_to to-list '_type type))))))

(define-syntax (link stx)
  (syntax-case stx ()
    ((_ type (from to))
      (with-syntax ((generated-name #'(new-name)))
        #'(add-link type 'from 'to generated-name)))
    ((_ type name (from to)) #'(add-link type 'from 'to 'name))))

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

(define-syntax-rule (perturbing-agent name)
  (node 'name 'perturbing-agent))

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

; evaluation
(define-syntax experiment
  (syntax-rules (start)
    ((experiment h (start nodes ...) body ...)
      (let ((__er h)
            (__count-steps 0)
            (__next-process-list '()))
        (syntax-parameterize
          ((ER (make-rename-transformer #'__er))
          (COUNT-STEPS (make-rename-transformer #'__count-steps))
          (PROCS (make-rename-transformer #'__next-process-list)))
            (let* ((start-nodes '(nodes ...))
                  (enter-nodes (get-enter-nodes ER)))
              ; initialize system:
              (set! ER (make-neurones ER #:state 'exists))
              (set! COUNT-STEPS (* 5 (length (hash-keys ER))))
              ; send state through the network:
              (set! PROCS
                (if (empty? start-nodes)
                  (map
                    (λ (node)
                        (λ (h) (calculate-state h node #:in-signal 'not-exists)))
                    enter-nodes)
                  (map
                    (λ (node)
                        (λ (h) (calculate-state h node #:in-signal 'exists)))
                    start-nodes)))
              (pick-process-iter)
              ;(set! ER (hash-map
              ;            (λ (k v)
              ;              (let ((state (hash-ref v 'state 'exists)))
              ;                (values k (hash-substitute v (cons 'state state)))))
              ;            ER))
              body ...
              ))))))

; print environment hash
(define-syntax (probe stx)
  (syntax-case stx (state :)
    ((_)
      #'ER)
    ((_ object-name)
      #'(hash-ref ER 'object-name))
    ((_ object-name state)
      #'(hash-ref (hash-ref ER 'object-name) 'state))
    ((_ object-name : varname)
      #'(hash-ref (hash-ref ER 'object-name) 'varname))))

; print environment hash
(define-syntax (probe-print stx)
  (syntax-case stx (state :)
    ((_)
      #'ER)
    ((_ object-name)
      #'(hash-ref ER 'object-name))
    ((_ object-name state)
      #'(printf "~a: ~a~n" 'object-name (hash-ref (hash-ref ER 'object-name) 'state)))
    ((_ object-name : varname)
      #'(printf "~a: ~a~n" 'object-name (hash-ref (hash-ref ER 'object-name) 'varname)))))

; pick random lambda to propagate state until either
; - no more lambdas in the list
; - number of steps reached their limit
(define-syntax-rule (pick-process-iter)
  (for ((i (in-naturals)))
        #:break (or (empty? PROCS) (<= COUNT-STEPS 0))
        (begin
          (set! COUNT-STEPS (dec COUNT-STEPS))
          (let ((next-proc (list-ref PROCS (random (length PROCS)))))
            (let-values (((er new-process-lambdas) (next-proc ER)))
              ;(--- (hash-ref ER 'Sense))
              (set! ER er)
              (set! PROCS (append (exclude PROCS next-proc) new-process-lambdas))
              )))))

(define (node? el)
  (case (hash-ref el '_type #f)
    ((entity perturbing-agent) #t)
    (else #f)))

(define (link? el)
  (case (hash-ref el '_type #f)
    ((interaction modulation stimulation necessary-stimulation absolute-stimulation inhibition absolute-inhibition) #t)
    (else #f)))

(define (reprocial? el)
  (case (hash-ref el '_type #f)
    ((interaction) #t)
    (else #f)))

(define (element? el)
  (or (node? el) (link? el)))

(define (assignment? el)
  (equal? (hash-ref el '_type #f) 'assignment))

(define (link+? el)
  (or (link? el) (assignment? el)))

(define (compound-name? id)
  (ormap (λ (x) (indexof? (symbol->string id) x)) '(":")))

(define (parse-node-form id)
  (let ((res (map string->symbol (string-split (symbol->string id) #rx":"))))
    (cons
      (car res)
      (if (empty? (cdr res)) #f (cadr res)))))

(define (node-pure-name id)
  (car (parse-node-form id)))

(define (node-var-name id)
  (format-symbol ":~a" (cdr (parse-node-form id))))

(define (element-type element)
  (hash-ref element '_type #f))

(define (get-enter-nodes h)
  (let-values (((tos froms)
                  (for/fold
                    ((accto empty) (accfrom empty))
                    (((k v) h) #:when (element? v))
                      (let* ((to (hash-ref v '_to null))
                            (to (if (list? to) to (list to)))
                            (from (hash-ref v '_from null))
                            (from (if (list? from) from (list from)))
                            (to (if (reprocial? v) (append to from) to))
                            (from (if (reprocial? v) (append from to) from)))
                        (values
                          (append accto to)
                          (append accfrom from))))))
        (let* ((tos (map node-pure-name (uniques tos)))
              (froms (map node-pure-name (uniques froms))))
          (hash-keys
            (hash-filter
              (λ (element-key element-value)
                (and
                  (node? element-value)
                  (indexof? froms element-key)
                  (not (indexof? tos element-key))))
              h)))))

(define-catch (make-neurones h #:state (state 'exists))
  (for/fold
    ((h-res h))
    ((cur-k (hash-keys h)))
    (let*
        ((element (hash-ref h cur-k))
        (tos (hash-ref element '_to empty))
        (pured-tos (map node-pure-name tos))
        (axones
          (hash-keys
            (hash-filter
              (λ (k v)
                (and
                  (link? v)
                  (equal? (hash-ref v '_from #f) cur-k)))
              h)))
        (synapses (filter
                    (λ (x) (link+? (hash-ref h x)))
                    pured-tos))
        (synapses (if (assignment? element)
                        pured-tos
                        synapses))
        (vars (filter (λ (x) (re-matches? "^:.+" (symbol->string x))) (hash-keys element)))
        (vars-bases-hash (for/hash ((e vars)) (values (format-symbol "~a-base" e) (hash-ref element e)))))
          (hash-substitute
                h-res
                (cons
                  cur-k
                  (hash-union
                    (hash-ref h-res cur-k)
                    (hash 'state state)
                    vars-bases-hash
                    (hash
                      'state state
                      'axones axones
                      'synapses synapses)))))))

(define-catch (calculate-state h element-key #:in-signal (in-signal 'exists))
  (let ((element (hash-ref h element-key)))
    (cond
      ((link+? element) (calculate-state-link h element-key #:in-signal in-signal))
      ((node? element) (calculate-state-node h element-key #:in-signal in-signal))
      (else (values h null)))))

(define-catch (calculate-state-node h element-key #:in-signal (in-signal 'exists))
  (let* (
        (element (hash-ref h element-key))
        (state (hash-ref element 'state))
        (new-state (case in-signal
                  ((exists) 'exists)
                  ((not-exists) 'not-exists)
                  (else state)))
        (axones (hash-ref element 'axones))
        (synapses (hash-ref element 'synapses))
        (var-change? (hash? in-signal))
        )
          (if var-change?
            (let* ((var-name (car (hash-keys in-signal)))
                  (var-value (hash-ref element var-name #f))
                  (var-signal-value (hash-ref in-signal var-name))
                  (var-base-value (hash-ref element (format-symbol "~a-base" var-name)))
                  (var-new-value (case var-signal-value
                                        ((reset) var-base-value)
                                        ((nop) var-value)
                                        (else var-signal-value)))
                  (exist-var-change? (equal? var-name ':exist))
                  (new-state (if exist-var-change?
                                (if (hash-ref element ':exist)
                                  'exists
                                  'not-exists)
                                state))
                  (axone-out-signal (get-axone-signal new-state 'node))
                  (synapse-out-signal (get-synapse-signal new-state 'node))
                  (update (list (cons var-name var-new-value) (cons 'state new-state)))
                  (h-new (hash-substitute
                            h
                            (cons
                              element-key
                              (hash-substitute
                                element
                                update))))
                  (new-process-lambdas
                    (append
                      (map (λ (axone)
                        (λ (h) (calculate-state h-new axone #:in-signal axone-out-signal))) axones)
                      (map (λ (synapse)
                        (λ (h) (calculate-state h-new synapse #:in-signal synapse-out-signal))) synapses))))
              ;(--- type ":" in-signal "->" state new-state "->" synapse-out-signal axone-out-signal)
              (values h-new new-process-lambdas))
            (let* ((axone-out-signal (get-axone-signal new-state 'node))
                  (synapse-out-signal (get-synapse-signal new-state 'node))
                  (h-new (if (not (equal? state new-state))
                            (hash-substitute
                              h
                              (cons
                                element-key
                                (hash-substitute
                                  element
                                  (cons 'state new-state))))
                            h))
                  (new-process-lambdas
                    (append
                      (map (λ (axone)
                        (λ (h) (calculate-state h-new axone #:in-signal axone-out-signal))) axones)
                      (map (λ (synapse)
                        (λ (h) (calculate-state h-new synapse #:in-signal synapse-out-signal))) synapses))))
              ;(--- type ":" in-signal "->" state "->" synapse-out-signal axone-out-signal)
              (values h-new new-process-lambdas)))))

(define-catch (calculate-state-link h element-key #:in-signal (in-signal 'exists))
  (let* (
        (element (hash-ref h element-key))
        (type (hash-ref element '_type))
        (to (hash-ref element '_to))
        (to (if (list? to) (car to) to))
        (from (hash-ref element '_from))
        (axones (hash-ref element 'axones))
        (synapses (hash-ref element 'synapses))
        (state (hash-ref element 'state #f))
        (old-state (if state state 'exists)) ; maybe not!
        (new-state (get-new-state in-signal old-state))
        (synapse-out-signal (get-synapse-signal new-state type))
        ; if assignment:
        (synapse-out-signal (case synapse-out-signal
                              ((reset) (hash (node-var-name to) 'reset))
                              ((new-value) (hash (node-var-name to) from))
                              ((nop) (if (equal? type 'assignment)
                                            (hash (node-var-name (car to)) 'nop)
                                            synapse-out-signal))
                              (else synapse-out-signal)))
        (axone-out-signal (get-axone-signal new-state 'link))
        (new-element-value (cons 'state new-state))
        (h-new (hash-substitute
                  h
                  (cons
                    element-key
                    (hash-substitute
                      element
                      new-element-value))))
        (new-process-lambdas
          (append
            (map (λ (synapse)
                    (λ (h) (calculate-state h-new synapse #:in-signal synapse-out-signal))) synapses)
            (map (λ (axone)
                    (λ (h) (calculate-state h-new axone #:in-signal axone-out-signal))) axones)))
        )
          ;(--- type ":" in-signal "->" old-state new-state "->" synapse-out-signal axone-out-signal)
          (values h-new new-process-lambdas)
))
