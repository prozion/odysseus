#lang racket

(require "../lib/_all.rkt")
(require racket/syntax)

(provide (all-defined-out))

(define ELEMENTS
  (hash
    'H (hash 'm 1)

    'Li (hash 'm 7)
    'B (hash 'm 11)
    'C (hash 'm 12)
    'N (hash 'm 14)
    'O (hash 'm 16)
    'F (hash 'm 19)

    'Na (hash 'm 23)
    'Ma (hash 'm 24)
    'Al (hash 'm 27)
    'Si (hash 'm 28)
    'P (hash 'm 31)
    'S (hash 'm 32)
    'Cl (hash 'm 35)

    'K (hash 'm 39)
    'Ca (hash 'm 40)

    'Fe (hash 'm 56)

    'Se (hash 'm 79)
    'Br (hash 'm 80)
  ))

(define GROUPS
  (hash
    'OH '(H O)
    'NH2 '(N H H)))

(define (get-formula element-ctx)
  (let ((expr ($ f element-ctx)))
    (read (open-input-string expr))))

(define (get-element-name e)
  (->symbol
    (first
      (string-split
        (first (string-split (->string e) ":"))
        "/"))))

(define (binding-form e)
  (match e
    (`(,bindings ,part)
        #:when (re-matches? "\\..+?" (->string bindings))
        e)
    (else #f)))

(define-catch (binding-form? e)
  (true? (binding-form e)))

(define (outbinding? e)
  (starts-with? (->string e) "."))

(define (get-outbinds e)
  (if (binding-form? e)
    (symbol-split (first (binding-form e)) ".")
    empty))

(define (get-binded-element e)
  (second e))

(define (get-binded-element-name e)
  (first
    (symbol-split (get-binded-element e) ".")))

(define (get-inbinds e)
  (rest
    (symbol-split (get-binded-element e) ".")))

(define (parse-element e)
  (let ((e-str (->string e)))
    (match e-str
      ((regexp #rx".+?\\..+?")
          (let* ((parsed-e (string-split e-str "."))
                (core (->symbol (first parsed-e)))
                (inbinds (->symbol (rest parsed-e))))
            (hash-union (parse-element core) (hash 'inbinds inbinds))))
      ((regexp #rx".+?:.+?")
          (let* ((parsed-e (string-split e-str ":"))
                (core (->symbol (first parsed-e)))
                (ref (->symbol (second parsed-e))))
            (hash-union (parse-element core) (hash 'element-reference ref))))
      ((regexp #rx".+?/.+?")
          (let* ((parsed-e (string-split e-str "/"))
                (element-name (->symbol (first parsed-e)))
                (placeholder-id (->symbol (second parsed-e))))
            (hash 'element-name element-name 'placeholder-id placeholder-id)))
      (else (hash 'element-name (->symbol e-str))))))

(define (element-with-reference? e)
  (has-key? (parse-element e) 'element-reference))

(define (element-with-bindable-position? e)
  (has-key? (parse-element e) 'placeholder-id))

(define (get-bindable-position-id e)
  ($ placeholder-id (parse-element e)))

(define (element-with-inbinds? e)
  (has-key? (parse-element e) 'inbinds))

(define (reference? e)
  (starts-with? (->string e) ":"))

(define (special-symbol? e)
  (indexof? '(- = -=) e))

(define (chained-form? form)
  (and
    (list? form)
    (equal? (first form) '-)))

(define (basic-element? e-name)
  (has-key? ELEMENTS e-name))

(define (element-group? e-name)
  (has-key? GROUPS e-name))

(define (get-group-molecule-form group-name)
  (hash-ref GROUPS group-name empty))

(define (alias? e)
  (and
    (scalar? e)
    (let* ((parsed-e (parse-element e))
          (e-name ($ element-name parsed-e)))
      (and
        (not (basic-element? e-name))
        (not (element-group? e-name))))))

(define (get-atom-mass e-name)
  (let* ((element-ctx (hash-ref ELEMENTS e-name #f))
        (m (and element-ctx ($ m element-ctx))))
    (or m
      (errorf "unrecognized element name: ~a" e-name))))

(define-catch (get-molecule-mass molecule-form #:aliases (aliases-h #f) #:bindings (bindings #f))
  (--- molecule-form bindings)
  (let* ((binding-positions (and
                              (list? molecule-form)
                              (flatten
                                (map
                                  get-outbinds
                                  (filter binding-form? molecule-form)))))
        (binding-positions (if (and binding-positions bindings) (append bindings binding-positions) binding-positions))
        (binded? (λ (binding-position-name) (indexof? binding-positions binding-position-name)))
        (molecule-form (if (scalar? molecule-form) (list molecule-form) molecule-form)))
    (for/fold
      ((mass 0))
      ((e molecule-form))
      (cond
        ((empty? e) (--- -1) 0)
        ; -
        ((special-symbol? e) (--- 0) mass)
        ; :c1
        ((reference? e) (--- 1) mass)
        ; (.a1.a2 B.b1.b2)
        ((binding-form? e) (--- 2) (+ mass (get-molecule-mass (get-binded-element-name e) #:aliases aliases-h #:bindings (get-inbinds e))))
        ; (... (C OH H -) ...)
        ((list? e) (--- 3) (+ mass (get-molecule-mass e #:aliases aliases-h #:bindings bindings)))
        ; OH/b1
        ((element-with-bindable-position? e)  (--- 4 e binding-positions bindings)
                                (if (binded? (get-bindable-position-id e))
                                  mass
                                  (+ mass (get-molecule-mass (get-element-name e)))))
        ; NH2
        ((element-group? e) (--- 5) (+ mass (get-molecule-mass (get-group-molecule-form e))))
        ; C:c1
        ((element-with-reference? e) (--- 6) (+ mass (get-atom-mass (get-element-name e))))
        ; N
        ((basic-element? e) (--- 7) (+ mass (get-atom-mass e)))
        ; ATP
        (aliases-h (--- 8) (+ mass (get-molecule-mass (hash-ref aliases-h e) #:aliases aliases-h #:bindings bindings)))
        (else (errorf "element cannot be recognized: ~a" e))))))
