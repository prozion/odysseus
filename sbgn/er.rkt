#lang racket

(require "../lib/all.rkt")
(require racket/stxparam)
(require (for-syntax racket/syntax "../lib/seqs.rkt" "../lib/controls.rkt"))

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

(define-syntax-rule (node name kind parameters ...)
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
          (hash '_kind kind 'charge #f))))))

(define-syntax-rule (add-link kind from to name)
  (let ((to-list (if (list? to) to (list to))))
    (set!
      ER
      (hash-insert
        ER
        (cons
          name
          (hash '_from from '_to to-list '_kind kind))))))

(define-syntax (link stx)
  (syntax-case stx ()
    ((_ kind (from to))
      (with-syntax ((generated-name #'(new-name)))
        #'(add-link kind 'from 'to generated-name)))
    ((_ kind name (from to)) #'(add-link kind 'from 'to 'name))))

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
            (begin
              ; initialize system:
              (set! ER (make-neurones ER #:charge 1))
              (set! COUNT-STEPS (* 3 (length (hash-keys ER))))
              ; send charge through the network:
              (set! PROCS
                (map
                  (λ (node)
                      (λ (h) (propagate-charge h node #:start? #t)))
                  '(nodes ...)))
              (pick-process-iter)
              (set! ER (hash-map
                          (λ (k v)
                            (let ((charge (hash-ref v 'charge #f)))
                              (values k (hash-substitute v (cons 'charge (resolve-state charge))))))
                          ER))
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
      #'(hash-ref (hash-ref ER 'object-name) 'charge))
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
      #'(printf "~a: ~a~n" 'object-name (hash-ref (hash-ref ER 'object-name) 'charge)))
    ((_ object-name : varname)
      #'(printf "~a: ~a~n" 'object-name (hash-ref (hash-ref ER 'object-name) 'varname)))))

; pick random lambda to propagate charge until either
; - no more lambdas in the list
; - number of steps reached their limit
(define-syntax-rule (pick-process-iter)
  (for ((i (in-naturals)))
        #:break (or (empty? PROCS) (<= COUNT-STEPS 0))
        (begin
          (set! COUNT-STEPS (dec COUNT-STEPS))
          (let ((next-proc (list-ref PROCS (random (length PROCS)))))
            (let-values (((er new-process-lambdas) (next-proc ER)))
              (set! ER er)
              (set! PROCS (append (exclude PROCS next-proc) new-process-lambdas))
              )))))

(define (node? el)
  (case (hash-ref el '_kind #f)
    ((entity perturbing-agent) #t)
    (else #f)))

(define (link? el)
  (case (hash-ref el '_kind #f)
    ((interaction modulation stimulation necessary-stimulation absolute-stimulation inhibition absolute-inhibition) #t)
    (else #f)))

(define (assignment? el)
  (equal? (hash-ref el '_kind #f) 'assignment))

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

(define (points-to-var-name id)
  (cdr (parse-node-form id)))

(define (element-type element)
  (hash-ref element '_kind #f))

(define-catch (make-neurones h #:charge (charge #f))
  (for/fold
    ((h-res h))
    ((cur-k (hash-keys h)))
    (let*
        ((element (hash-ref h cur-k))
        (tos (hash-ref element '_to empty))
        (pured-tos (map node-pure-name tos))
        (dendrones
          (hash-keys
            (hash-filter
              (λ (k v)
                (and
                  (link? v)
                  (indexof? (hash-ref v '_to empty) cur-k)))
              h)))
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
        (points-to-var (if (assignment? element) (car (map points-to-var-name tos)) #f)))
          (hash-substitute
                h-res
                (cons
                  cur-k
                  (hash-union
                    (hash-ref h-res cur-k)
                    (hash
                      'charge charge
                      'dendrones dendrones
                      'axones axones
                      'synapses synapses
                      'points-to-var points-to-var)))))))

(define-catch (propagate-charge h element-key #:delta-charge (delta-charge 0) #:start? (start? #f) #:var (var #f))
  (let ((element (hash-ref h element-key)))
    (cond
      ((link+? element) (propagate-charge-link h element-key #:delta-charge delta-charge #:start? start?))
      ((node? element) (propagate-charge-node h element-key #:delta-charge delta-charge #:start? start? #:var var))
      (else (values h null)))))

(define-catch (propagate-charge-node h element-key #:delta-charge (delta-charge 0) #:start? (start? #f) #:var (var #f))
  (let* (
        (element (hash-ref h element-key))
        (axones (hash-ref element 'axones))
        (axone-delta-charge 1)
        (var-name (string->symbol (format ":~a" var)))
        (var-value (hash-ref element var-name #f))
        (new-element-value (if var (cons var-name delta-charge) (cons var-name var-value)))
        (h-new (hash-substitute
                  h
                  (cons
                    element-key
                    (hash-substitute
                      element
                      new-element-value))))
        (new-process-lambdas
            (map (λ (axone)
              (λ (h) (propagate-charge h axone #:delta-charge axone-delta-charge))) axones)))
    (values h-new new-process-lambdas)))

(define-catch (propagate-charge-link h element-key #:delta-charge (delta-charge 0) #:start? (start? #f))
  (let* (
        (element (hash-ref h element-key))
        (kind (hash-ref element '_kind))
        (points-to-var (hash-ref element 'points-to-var #f)) ; name of variable at to-element
        (dendrones (hash-ref element 'dendrones))
        (axones (hash-ref element 'axones))
        (synapses (hash-ref element 'synapses))
        (charge (if start? 100 (hash-ref element 'charge)))
        (charge (cond
                  ((not delta-charge) charge) ; don't propagate uncertainties (or try propagation too?)
                  ((= delta-charge 0) charge) ; no stimulation through dendrones
                  ((>= delta-charge 50) ; absolute activation
                    (cond
                      ((not charge) 100) ; absolute activation versus undefined state -> suppose activated state
                      ((<= charge -50) #f) ; absolute activation versus blocked state -> undefined state
                      ((>= charge 50) 100) ; absolute activation adds to activated state -> still activated state
                      ((> 0 charge -50) 100) ; absolute activation versus inhibited state -> activated state
                      ((< 0 charge 50) 100) ; absolute activation versus catalyzed state -> activated state
                      ((= charge 0) 100) ; absolute activation versus blank state -> suppose activated state
                      ))
                  ((<= delta-charge -50) ; absolute inhibition
                    (cond
                      ((not charge) -100) ; absolute inhibition versus undefined state -> suppose blocked state
                      ((<= charge -50) -100) ; absolute inhibition versus blocked state -> blocked state
                      ((>= charge 50) #f) ; absolute inhibition versus activated state -> undefined state
                      ((> 0 charge -50) -100) ; absolute inhibition versus inhibited state -> blocked state
                      ((< 0 charge 50) -100) ; absolute inhibition versus catalyzed state -> blocked state
                      ((= charge 0) -100) ; absolute inhibition versus blank state -> blocked state
                      ))
                  ((> 50 delta-charge -50) ; inhibition or catalyzation = influence
                    (cond
                      ((not charge) #f) ; influence versus undefined state -> undefined state
                      ((<= charge -50) -100) ; influence versus blocked state -> blocked state
                      ((>= charge 50) 100) ; influence versus activated state -> activated state
                      ((> 0 charge -50) (+ delta-charge charge)) ; influence versus inhibited state -> influenced state
                      ((< 0 charge 50) (+ delta-charge charge)) ; influence versus catalyzed state -> influenced state
                      ((= charge 0) delta-charge) ; influence versus blank state -> influenced state
                      ))))
        (synapse-delta-charge
                (case kind
                  ((stimulation)
                    (cond
                      ((not charge) 0)
                      ((< charge -10) 0)
                      ((>= charge -10) 1)))
                  ((absolute-stimulation)
                    (cond
                      ((not charge) #f)
                      ((< charge -10) 0)
                      ((>= charge -10) 100)))
                  ((neccessary-stimulation)
                    (cond
                      ((not charge) -100)
                      ((< charge -10) -100)
                      ((>= charge -10) 100)))
                  ((absolute-inhibition)
                    (cond
                      ((not charge) #f)
                      ((< charge -50) 0)
                      ((<= -50 charge 0) -100)
                      ((> charge 0) -100)))
                  ((inhibition)
                    (cond
                      ((not charge) 0)
                      ((< charge -10) 0)
                      ((<= -50 charge 0) 0)
                      ((> charge 0) -1)))
                  ((assignment)
                    (let ((value-to-set (hash-ref element '_from)))
                      (cond
                        ((not charge) #f)
                        ((< charge -10) #f)
                        ((<= -50 charge 0) #f)
                        ((> charge 0) value-to-set))))
                  (else 0)))
        ; equivalent to neccessary stimulation
        (axone-delta-charge
                (cond
                  ((not charge) -100)
                  ((< charge -10) -100)
                  ((>= charge -10) 100)))
        (new-element-value (cons 'charge charge))
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
                    (λ (h) (propagate-charge h synapse #:delta-charge synapse-delta-charge #:var points-to-var))) synapses)
            (map (λ (axone)
                    (λ (h) (propagate-charge h axone #:delta-charge axone-delta-charge))) axones)))
        )
          (values h-new new-process-lambdas)
))

(define (digitize-state state)
  (case state
    ((active) 1)
    ((blocked) -1)
    ((nonstable) 0)
    ((undefined) #f)
    (else state)))

(define (resolve-state state)
  (cond
    ((not state) 'undefined)
    ((> state 0) 'active)
    ((< state 0) 'blocked)
    ((= state 0) 'nonstable)))
