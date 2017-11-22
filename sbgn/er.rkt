#lang racket

(require "../lib/all.rkt")
(require racket/stxparam)
(require "er-rules.rkt")

(provide (all-defined-out))

(define repeat-factor 2)

(define-syntax-parameter ER
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use ER inside 'sbgn:er'")))

(define-syntax-parameter NAMES
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "you must use NAMES inside 'sbgn:er'")))

(define-syntax-rule (sbgn:er body ...)
  (let ((er (hash))
        (names (list)))
    (syntax-parameterize ((ER (make-rename-transformer #'er))
                          (NAMES (make-rename-transformer #'names)))
          ((λ () body ...))
          (set! ER (form-elements-graph ER))
          ER)))

(define-syntax-rule (add-element name_ type_ from_ to_ parameters_ ...)
  (let ((lst-to_ (if (list? to_) to_ (list to_)))
        (lst-from_ (if (list? from_) from_ (list from_))))
    (set!
      ER
      (hash-insert
        ER
        (cons
          name_
          (hash-union
            (if (empty? '(parameters_ ...))
              (hash)
              (apply hash '(parameters_ ...)))
            (hash 'type type_ 'state 'exists 'from lst-from_ 'to lst-to_)))))))

(define-syntax (element stx)
  (syntax-case stx (parameters)
    ((_ type (parameters params_ ...))
      (with-syntax ((generated-name #'(new-name)))
        #'(add-element generated-name type empty empty params_ ...)))
    ((_ type name (parameters params_ ...))
        #'(add-element 'name type empty empty params_ ...))
    ((_ type (from to) params_ ...)
      (with-syntax ((generated-name #'(new-name)))
        #'(add-element generated-name type 'from 'to params_ ...)))
    ((_ type name (from to) params_ ...)
      #'(add-element 'name type 'from 'to params_ ...))
    ((_ type name)
        #'(add-element 'name type empty empty))))

;; diagram elements
(define-syntax-rule (entity name parameters ...)
  (element 'entity name parameters ...))

(define-syntax-rule (perturbing-agent name)
  (element 'perturbing-agent name))

(define-syntax-rule (interaction args ...)
  (element 'interaction args ...))

(define-syntax-rule (modulation args ...)
  (element 'modulation args ...))

(define-syntax-rule (stimulation args ...)
  (element 'stimulation args ...))

(define-syntax-rule (necessary-stimulation args ...)
  (element 'necessary-stimulation args ...))

(define-syntax-rule (absolute-stimulation args ...)
  (element 'absolute-stimulation args ...))

(define-syntax-rule (inhibition args ...)
  (element 'inhibition args ...))

(define-syntax-rule (absolute-inhibition args ...)
  (element 'absolute-inhibition args ...))

(define-syntax-rule (assignment args ...)
  (element 'assignment args ...))

(define-catch (form-elements-graph h)
  (local ((define (pointers-collector pointer)
            (λ (key)
              (for/fold
                ((acc (list)))
                ((next-element-key (hash-keys h)))
                (let* ((next-element-value (hash-ref h next-element-key))
                      (pointers (hash-ref next-element-value pointer)))
                  (if (and (indexof? pointers key) (not (indexof? acc key)))
                    (pushr acc next-element-key)
                    acc)))))
          (define axones (pointers-collector 'from))
          (define dendrones (pointers-collector 'to))
          (define (form-elements-graph-iter h-rest h-result)
            (cond
              ((empty? (hash-keys h-rest)) h-result)
              (else
                (let* ((cur-key (car (hash-keys h-rest)))
                      (cur-value (hash-ref h-rest cur-key))
                      (initial-parameters
                        (map
                          (λ (x) (cons (format-symbol "~a0" x) (hash-ref cur-value x)))
                          (hash-keys cur-value))))
                  (form-elements-graph-iter
                    (hash-delete h-rest cur-key)
                    (hash-insert
                      h-result
                      (cons
                        cur-key
                        (hash-insert
                          cur-value
                          (append
                            initial-parameters
                            (list
                              (cons 'dendrones (dendrones cur-key))
                              (cons 'axones (axones cur-key)))))))))))))
    (form-elements-graph-iter h (hash))))

;; evaluation
(define-syntax (experiment stx)
  (syntax-case stx (start)
    ((_ h (start nodes ...) body ...)
      #'(let ((er h))
        (syntax-parameterize
          ((ER (make-rename-transformer #'er)))
            (let* ((count-steps (* repeat-factor (hash-count h)))
                    (start-nodes '(nodes ...))
                    (start-signals (map (λ (x) (cons x 'exists)) start-nodes))
                    (enter-nodes (get-enter-nodes h))
                    (turn-off-nodes (minus enter-nodes start-nodes))
                    (turn-off-signals (map (λ (x) (cons x 'not-exists)) turn-off-nodes))
                    (signals-queue (append start-signals turn-off-signals)))
                (set! ER (change-state-iter h signals-queue count-steps))
                body ...))))))

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

(define-catch (get-enter-nodes elements-graph)
  (hash-keys
    (hash-filter
      (λ (k v)
        (let ((axones (hash-ref v 'axones))
              (dendrones (hash-ref v 'dendrones)))
          ;(--- k axones dendrones)
          (and
            (node? v)
            (not (empty? axones))
            ; should be no any interactions in the outgoing edges:
            (empty?
              (filter
                (λ (x) (equal? (get-type elements-graph x) 'interaction))
                (append axones dendrones)))
            (empty? dendrones))))
      elements-graph)))

(define-catch (change-state-iter elements-graph signals-queue count-steps)
  (cond
    ((<= count-steps 0) elements-graph)
    ((empty? signals-queue) elements-graph)
    (else
      (let* ((signal (car signals-queue))
            (key (car signal))
            (var-name (and (compound-name? key) (node-var-name key)))
            (key (if (compound-name? key) (node-pure-name key) key))
            (command (cdr signal))

            (element (hash-ref elements-graph key))
            (element-type (hash-ref element 'type))

            ; accept value assignment
            (var-value
              (and var-name command))
            (var-value (cond
                          ((equal? var-value 'reset)
                            (hash-ref element (format-symbol "~a0" var-name)))
                          ((equal? var-value 'nop) (hash-ref element var-name))
                          (else var-value)))

            (old-state (hash-ref element 'state))
            (new-state (get-new-state command old-state))
            (new-state (if (equal? var-name ':exist)
                          (if var-value 'exists 'not-exists)
                          new-state))

            (axones (hash-ref element 'axones))
            (axone-signal-type (get-axone-signal new-state))
            (axone-signals (map (λ (x) (cons x axone-signal-type)) axones))


            (synapses (hash-ref element 'to))
            (synapse-signal-type (get-synapse-signal new-state element-type))
            ; in assignment put assigned value into the signal:
            (synapse-signal-type (if (equal? synapse-signal-type 'new-value)
                                    (car (hash-ref element 'from))
                                    synapse-signal-type))
            (synapse-signal (map (λ (x) (cons x synapse-signal-type)) synapses))

            ;(_ (--- key "---" new-state "---" axone-signals synapse-signal))

            (updated-element (hash-update element 'state (λ (v) new-state)))
            (updated-element (if var-name
                                (hash-update updated-element var-name (λ (v) var-value))
                                updated-element))
            (updated-elements-graph (hash-update elements-graph key (λ (v) updated-element))))
        (change-state-iter
          updated-elements-graph
          (append
            (cdr signals-queue)
            (append axone-signals synapse-signal))
          (dec count-steps))))))

;; helpers
(define (node? el)
  (case (hash-ref el 'type #f)
    ((entity perturbing-agent) #t)
    (else #f)))

(define (get-type h key)
  (hash-ref (hash-ref h key) 'type))

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

; generate new random unique name
(define-syntax (new-name stx)
  (syntax-case stx ()
    ((_)
      #'(letrec ((new-name-recur
                  (λ (h (n #f))
                    (let ((next-name (string->symbol (random-word 7 #:prefix "_"))))
                      (cond
                        ((not n) (new-name-recur h next-name))
                        ((indexof? h n) (new-name-recur h next-name))
                        (else n)))))
              (n (new-name-recur NAMES)))
          (set! NAMES (pushr NAMES n))
          n))))
