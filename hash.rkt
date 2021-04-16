#lang racket

(require compatibility/defmacro)
(require "base.rkt" "seqs.rkt" "type.rkt" "regexp.rkt" "debug.rkt" "controls.rkt" "strings.rkt" (for-syntax "seqs.rkt" "type.rkt" racket/list racket/string))

(provide (all-defined-out))

;; INIT
(define (@ . body)
  ;(apply hash body))
  ;(apply hash (clean nil? body)))
  (let* ((l (length body))
        (body (if (even? l) body (cdr body))))
  (make-hash
    ; remove all pairs with nil-type keys:
    (clean (λ (x) (nil? (car x))) (hash->list (apply hash body))))))

(define-catch (get-$ path hh)
  (cond
    ((empty? hh) empty)
    ((empty? path) hh)
    ((hash? hh) (get-$ (rest path) (hash-ref* hh (first path))))
    ((and (list? hh) (one-element? hh)) (get-$ (rest path) (hash-ref* (first hh) (first path))))
    ((list? hh)
      (let ((res (cleanmap (map (λ (h) (get-$ path h)) hh))))
        (if (one-element? res)
          (first res)
          res)))
    (else #f)))

(define-macro ($ dotted-path hh)
  (let ((path (string-split (->string dotted-path) ".")))
    `(get-$ ',path ,hh)))

(define (get-$xml path hh)
  (let* ((res (get-$ path hh))
        (res (filter scalar? (flatten res)))
        (res (if (one-element? res) (first res) res))
        (res (if (empty? res) #f res)))
    res))

(define-macro ($xml dotted-path hh)
  (let ((path (string-split (->string dotted-path) ".")))
    `(get-$xml ',path ,hh)))

(define-macro (hash-sym . body)
  (let ((nbody (map (λ(x) (if (symbol? x) (symbol->string x) x)) body)))
    `(hash ,@nbody)))

(define (hash-key? h key)
  (true? (hash-ref h key #f)))

(define has-key? hash-key?)

(define (hashes? alst)
  (andmap hash? alst))

(define (not-hashes? alst) (not (hashes? alst)))

(define (hasher-by-names . body)
  (λ
    args
    (for/hash ((k body) (v args)) (values k v))))

; the same of hash-ref but no matter of type, matches if string projection values of hash key and compared key are equal
(define-catch (hash-ref* h key (default-value #f))
  (hash-ref
    (map-hash
      (λ (k v) (values (->string k) v))
      h)
    (->string key)
    default-value))

;; REDUCE
(define (hash-length h)
  (length (hash-keys h)))

(define (hash->alist h)
  (map
    (λ (x) (list (car x) (cdr x)))
    (hash->list h)))

(define (hash->ordered-list h keys-order)
  (for/list
    (((k v) (in-hash h))
    (i keys-order))
    (hash-ref h i #f)))

; '((foo 1 2 3 4) (bar 10 20 30)) -> #(foo:#(a:1 b:2 c:3) bar:#(a:10 b:20 c:30))
(define (list->hash lst header #:key-index (key-index 1) #:columns-exclude (columns-exclude null))
  (let* ((header (remove-by-pos header key-index)))
    (for/hash ((i lst))
      (values
        (nth i key-index)
        (hash-remove-keys
          (apply hash (interleave header (remove-by-pos i key-index)))
          columns-exclude)))))

(define (tree->hash lst)
  (let ((by-pairs (partition-full lst 2)))
    (for/fold
      ((res (hash)))
      ((pair by-pairs))
      (let ((key (first pair))
            (value (second pair)))
        (cond
          ((or (scalar? value) (plain-list? value))
            (hash-union res (apply hash pair)))
          (else
            (hash-union res (hash key (tree->hash value)))))))))

;; ACCESS
; (@. h.a.b.c)
(define-macro (@. path)
  (let* ((parts (map string->symbol (split (symbol->string path) ".")))
        (h (car parts))
        (rest (map (λ (x) `(quote ,x)) (cdr parts))))
    (cond
      ((null? rest) h)
      (else `(hash-path ,h ,@rest)))))

; (hash 'a (hash 'aa 10 'ab 20) 'b (hash 'ba (hash 'baa 300 'bab 30)))
; (hash-path h 'b 'ba 'bab) -> 30
(define (hash-path h . rest)
  (define (hash-path-r h path)
    (cond
      ((null? (cdr path)) (hash-ref h (car path) #f))
      (else (let ((calculated-hash (hash-path-r h (cdr path)))) ; if - to handle case when a path is deeper than hash
              (if (hash? calculated-hash)
                (hash-ref calculated-hash (car path) #f)
                #f)))))
  (hash-path-r h (reverse rest)))

(define (hash-pair h key (default #f))
  (for/fold
    ((res default))
    (((k v) h))
    (if (equal? k key)
        (cons k v)
        res)))

(define (hash-refs h keys (missed #f))
  (define (hash-refs-iter h keys res)
    (cond
      ((empty? keys) res)
      (else (hash-refs-iter
              h
              (cdr keys)
              (rpush
                res
                (let ((next-key (car keys)))
                (if (scalar? next-key)
                    (hash-ref h next-key missed)
                    (next-key h))))))))
  (hash-refs-iter h keys empty))

(define (hash-take h n)
  (let ((hl (hash-length h)))
    (cond
      ((>= n hl) h)
      (else (for/hash
              ( ((k v) (in-hash h))
                (i (in-range hl))
                #:break (= i n))
              (values k v))))))

;; FILTER
(define (@clean . body)
  (make-hash
    ;; remove all pairs with nil-type values:
    (clean (λ (x) (nil? (cdr x))) (hash->list (apply @ body)))))

(define (hash-filter lambdakv h)
  (for/fold
    ((res (hash)))
    (((k v) h))
    (if (lambdakv k v)
      (hash-insert res (cons k v))
      res)))

(define (hash-clean lambdakv h)
  (hash-filter (λ (k v) (not (lambdakv k v))) h))

(define hash-filter-not hash-clean)

(define (hash-regex-filter reg h)
  (make-hash
    (filter
      (λ (x)
        (let* (
                (key (car x))
                (key (cond
                      ((symbol? key) (symbol->string key))
                      ((string? key) key)
                      (else #f))))
          (if key
                (regexp-match reg key)
                #f)))
      (hash->list h))))

;; MAP
(define (map-hash f h)
  (for/hash (((k v) (in-hash h))) (f k v)))

(define (deep-map-hash f h)
  (for/hash (((k v) (in-hash h)))
    (if (hash? v)
      (values k (deep-map-hash f v))
      (f k v))))

;; MODIFY
(define (hash-delete h k)
  (cond
    ((immutable? h) (hash-remove h k))
    (else (hash-delete (make-immutable-hash (hash->list h)) k))))

(define (hash-delete-f h f)
  (cond
    ((immutable? h) (let ((ks (filter f (hash-keys h))))
                      (for/fold
                        ((res-h h))
                        ((k ks))
                        (hash-remove res-h k))))
    (else (hash-delete-f (make-immutable-hash (hash->list h)) f))))

(define (hash-delete-all h keys)
  (cond
    ((empty? keys) h)
    (else (hash-delete-all (hash-delete h (car keys)) (cdr keys)))))

; (h- h 'name 'state)
(define (h- h . keys)
  (cond
    ((empty? keys) h)
    (else (apply h- (hash-delete h (car keys)) (cdr keys)))))

(define (hash-substitute h1 arg)
  ;(printf "arg: ~a~n(car arg): ~a~nresulted hash: ~a~n" arg (car arg) (hash-delete h1 (car arg)))
  (cond
    ((null? arg) h1)
    ((simple-cons? arg)
      (hash-insert
        (hash-delete h1 (car arg))
        arg))
    ((cons-ext? arg)
      (hash-insert
        (hash-delete h1 (car arg))
        arg))
    ((list-of-cons? arg)
      (if (null? (cdr arg))
        (hash-substitute h1 (car arg))
        (hash-substitute (hash-substitute h1 (car arg)) (cdr arg))))
    (else h1)))

(define (hash-group-by h field)
  (for/fold
    ((s (hash)))
    (((k v) h))
    (hash-insert-fuse
      s
      (cons
        (hash-ref v field)
        (hash k v)))))

(define (fuse-values val1 val2)
  (cond
    ((and
        (hash? val1)
        (hash? val2))
          (hash-union val1 val2))
    ((and
        (list? val1)
        (list? val2))
          (merge-unique val1 val2))
    ((and
        (string? val1)
        (string? val2))
          (string-append val1 val2))
    (else
      val1)))

(define (hash-insert-fuse h1 pair)
  (cond
    ((not (pair? pair)) h1)
    ((not (hash? h1)) (hash (car pair) (cdr pair)))
    (else
      (let ((h1-part-v (hash-ref h1 (car pair) #f)))
        (cond
          ((not h1-part-v) (make-immutable-hash (cons pair (hash->list h1))))
          (else
            (hash-insert
              (hash-delete h1 (car pair))
              (cons
                (car pair)
                (fuse-values
                  (hash-ref h1 (car pair))
                  (cdr pair))))))))))

(define (hash-revert h)
  (apply hash (interleave (hash-values h) (hash-keys h))))

(define (hash-insert h1 pair #:fuse (overwrite #f))
  (cond
    ((null? pair) h1)
    ;((list-of-simple-cons? pair) (hash-insert (hash-insert h1 (car pair)) (cdr pair))) ; doesn't catch mixed lists of cons and lists
    ((list-of-seqs? pair) (hash-insert (hash-insert h1 (car pair) #:fuse overwrite) (cdr pair) #:fuse overwrite)) ; each element of list is either cons or list
    ((not (pair? pair)) h1)
    ((not (hash? h1)) (make-hash (list pair)))
    ((hash-ref h1 (car pair) #f) (if overwrite
                                    (hash-set h1 (car pair) (cdr pair))
                                    h1))
    (else
      (hash-set h1 (car pair) (cdr pair)))))

;; COMBINE
; add to resulting hash all key-val pairs from h1 and pairs from h2 with rest of the keys
(define (hash-union #:fuse (fuse #f) . hs)
  (cond
    ((empty? hs) (hash))
    ((one-element? hs) (first hs))
    ((two-elements? hs)
      (let* ((h1 (first hs))
            (h1 (or (and h1 (hash? h1) h1) (hash)))
            (h2 (second hs))
            (h2 (or (and h2 (hash? h2) h2) (hash))))
        (for/fold
          ((res h2))
          (((k1 v1) h1))
          (let (
                (v2 (hash-ref h2 k1 #f)))
            (cond
              (v2 (cond
                    ((equal? fuse 'combine)
                      (hash-set
                        res
                        k1
                        (cond
                          ((and (not v1) v2) v2)
                          ((and (scalar? v1) (scalar? v2))
                              (list v1 v2))
                          ((and (scalar? v1) (list? v2))
                              (pushr v2 v1))
                          ((and (list? v1) (scalar? v2))
                              (pushr v1 v2))
                          ((and (list? v1) (list? v2))
                              (append v1 v2))
                          ((and (scalar? v1) (hash? v2))
                              (list v1 v2))
                          ((and (hash? v1) (scalar? v2))
                              (list v1 v2))
                          ((and (hash? v1) (hash? v2))
                              (hash-union v1 v2 #:fuse 'combine))
                          (else
                            (error (format "mismatched value types for fusion: ~a and ~a (~a and ~a)" (type v1) (type v2) v1 v2))))))
                    ((equal? fuse 'combine-different)
                      (hash-set
                        res
                        k1
                        (cond
                          ((and (not v1) v2) v2)
                          ((equal? v1 v2) v1)
                          ((and (scalar? v1) (scalar? v2))
                              (list v1 v2))
                          ((and (scalar? v1) (list? v2))
                              (pushr v2 v1))
                          ((and (list? v1) (scalar? v2))
                              (pushr v1 v2))
                          ((and (list? v1) (list? v2))
                              (append v1 v2))
                          ((and (scalar? v1) (hash? v2))
                              (list v1 v2))
                          ((and (hash? v1) (scalar? v2))
                              (list v1 v2))
                          ((and (hash? v1) (hash? v2))
                              (hash-union v1 v2 #:fuse 'combine-different))
                          (else
                            (error (format "mismatched value types for fusion: ~a and ~a (~a and ~a)" (type v1) (type v2) v1 v2))))))
                    (else
                      (hash-set res k1 v1))))
              (else
                (hash-set res k1 v1)))))))
    (else
        (hash-union #:fuse fuse (first hs) (apply hash-union (rest hs))))))

; (define (hash-union #:fuse (overwrite #f) . hs)
;   (case (length hs)
;     ((1) (car hs))
;     ((2) (let* (
;               (h1 (car hs))
;               (h2 (cadr hs))
;               (h1 (if (nil? h1) (hash) h1))
;               (h2 (if (nil? h2) (hash) h2)))
;           (for/fold
;             ((res h1))
;             (([k v] (in-hash h2)))
;             (let ((v0 (hash-ref res k #f)))
;               (cond
;                 (overwrite
;                   (cond
;                     ((equal? overwrite 'combine)
;                       (let* ((v1 (hash-ref res k #f))
;                             (v2 v))
;                         (hash-set
;                           res
;                           k
;                           (cond
;                             ((and v1 (not v2)) v1)
;                             ((and v2 (not v1)) v2)
;                             ((and (not v1) (not v2)) #f)
;                             (else
;                               (append
;                                 (if (list? v1) v1 (list v1))
;                                 (if (list? v2) v2 (list v2))))))))
;                     ((or (true-sequence? v0) (true-sequence? v)) (hash-set res k (sequence-fuse v0 v #:fuse overwrite)))
;                     (else (hash-set res k v))))
;                 ((hash-ref res k #f) res)
;                 (else (hash-set res k v)))))))
;     (else (hash-union
;             #:fuse overwrite
;             (car hs)
;             (keyword-apply hash-union '(#:fuse) (list overwrite) (cdr hs))
;             ))))

; (define (hash-fuse . hs)
;   (keyword-apply hash-union '(#:fuse) '(fuse) hs))

; (define (sequence-fuse seq1 seq2 #:fuse (overwrite 'combine))
;   (cond
;     ((and (hash? seq1) (hash? seq2))
;       (case overwrite
;         ((#f)
;           seq1)
;         ((fuse)
;           (for/fold
;             ((res seq1))
;             (((k v2) (in-hash seq2)))
;             (let ((v1 (hash-ref seq1 k #f)))
;               (cond
;                 (v1
;                   (cond
;                     ((or (true-sequence? v1) (true-sequence? v2))
;                       (hash-set res k (sequence-fuse v1 v2 #:fuse 'combine)))
;                     (else
;                       res)))
;                 (else
;                   (hash-set res k v2))))))
;         ((fuse-overwrite)
;           (for/fold
;             ((res seq1))
;             (((k v2) (in-hash seq2)))
;             (let ((v1 (hash-ref seq1 k #f)))
;               (cond
;                 (v1
;                   (cond
;                     ((or (true-sequence? v1) (true-sequence? v2))
;                       (hash-set res k (sequence-fuse v1 v2 #:fuse 'combine-overwrite)))
;                     (else
;                       (hash-set res k v2))))
;                 (else
;                   (hash-set res k v2))))))
;         ((#t hard)
;           seq2)))
;     ((and (list? seq1) (list? seq2)
;       (case overwrite
;         ((#f) seq1)
;         ((fuse fuse-overwrite) (append seq1 seq2))
;         ((#t) seq2))))
;     ((and (list? seq1) (scalar? seq2))
;       (case overwrite
;         ((#f) seq1)
;         ((fuse fuse-overwrite) (pushr seq1 seq2))
;         ((#t) seq2)))
;     ((and (scalar? seq1) (list? seq2))
;       (case overwrite
;         ((#f) seq1)
;         ((fuse fuse-overwrite) (pushl seq2 seq1))
;         ((#t) seq2)))
;     ; ((and (list? seq1) (hash? seq2))
;     ;   (if overwrite
;     ;     seq2
;     ;     seq1))
;     ; ((and (hash? seq1) (list? seq2))
;     ;   (if overwrite
;     ;     seq2
;     ;     seq1))
;     (else
;       (case overwrite
;         ((#f fuse) seq1)
;         ((#t fuse-overwrite) seq2)))))

;; OUTPUT
(define-catch (hash->string
                h
                #:delimeter (delimeter ", ")
                #:list-delimeter (list-delimeter ",")
                #:prefix (prefix "")
                #:equal-sign (equal-sign "=")
                #:default-type (default-type 'plain)
                #:exclude-keys (exclude-keys #f)
                #:conversion-table (conversion-table #f))
  (let ((hl (hash-length h))
        (ks (hash-keys h)))
    (for/fold
      ((s ""))
      ((k ks) (i hl))
      (let* (
              (v (hash-ref h k))
              (vp (cond
                    ((list? v)
                      (let ((v (cond
                                  ((string? v) (map (λ (x) (format "\"~a\"" x)) v))
                                  (else (map (λ (x) (str (idfy x))) v)))))
                        (string-join v list-delimeter)))
                    ((string? v) (str "\"" v "\""))
                    (else (str (idfy v)))))
            (vp (if (and conversion-table (hash-ref conversion-table vp #f))
                      (hash-ref conversion-table vp)
                      vp)))
        (cond
          ((and exclude-keys (procedure? exclude-keys) (exclude-keys k v)) s)
          ((and exclude-keys (list? exclude-keys) (indexof? exclude-keys k)) s)
          ((< i (dec hl)) (format "~a~a~a~a~a~a" s prefix k equal-sign vp delimeter))
          (else (format "~a~a~a~a~a" s prefix k equal-sign vp)))))))

(define (hash-print-json h #:prefix (prefix ""))
  (format "{~a}"
          (hash->string h #:delimeter ", " #:prefix prefix #:equal-sign ": ")))

(define (print-hash format-str h)
  (for/fold
    ([res ""])
    ([(k v) (in-hash h)])
    (string-append res (format format-str k v))))

(define (hash-remove-keys h keys)
  (cond
    ((null? keys) h)
    (else (hash-remove-keys (hash-remove h (car keys)) (cdr keys)))))

(define-syntax (init-hash stx)
  (syntax-case stx ()
    ((_ (fields ...)) #'(make-hash (for/list ((f '(fields ...))) (cons f ""))))
    ((_ (fields ...) init-val) #'(make-hash (for/list ((f '(fields ...))) (cons f init-val))))))


; (define-syntax (hash-class stx)
;   (syntax-case stx ()
;     ('(classname)
;       #'(λ (args) (hash-union (hash 'type classname))
;     ('(classname (fields ...))
;       #'(hash-union (hash 'type classname) (init-hash fields)))
;     ('(classname ancestors ...)
;       #'(hash-union (hash 'type classname) ancestors ...))
;     ('(classname (fields ...) ancestors ...)
;       #'(hash-union (hash 'type classname) (init-hash fields) ancestors ...))))
;
; (define-macro (hash-object name class)
;   `(hash-union (hash 'name ,name) ,class))

(define (unify-hashes lst by-key)
  (let loop ((res empty) (lst-rest lst))
    (cond
      ((empty? lst-rest) res)
      (else
        (let* ((cur-hash (car lst-rest))
              (key-value (hash-ref cur-hash by-key #f))
              (with-same-key-value (if key-value
                                      (filter (λ (x) (equal? (hash-ref x by-key #f) key-value)) res)
                                      #f))
              (exist-with-same-key-value? (not (nil? with-same-key-value)))
              (with-same-key-value (if exist-with-same-key-value? (car with-same-key-value) #f))
              (joined-hash (if with-same-key-value
                              (apply hash-union (list with-same-key-value cur-hash))
                              cur-hash))
              (res (if exist-with-same-key-value?
                    (list-substitute res with-same-key-value joined-hash)
                    (pushr res joined-hash)))
              )
        (loop res (cdr lst-rest)))))))

(define-catch (hash-keys-substitute h original-keys subst-keys)
  (cond
    ((empty? subst-keys) h)
    ((empty? original-keys) h)
    (else
      (let* ((original-key (car original-keys))
            (subst-key (car subst-keys))
            (original-value (hash-ref h original-key))
            (h (hash-union (hash subst-key original-value) (hash-delete h original-key))))
        (hash-keys-substitute h (cdr original-keys) (cdr subst-keys))))))

(define (untyped-hash-ref h k)
  (hash-ref
    (map-hash
      (λ (k v) (values (->string k) v))
      h)
    (->string k)))

(define-catch (format-hash frmt h)
  (let* ((matches (get-matches #rx"{(.+?)}" frmt)))
    (for/fold
      ((res frmt))
      ((m matches))
      (string-replace res (first m) (->string (hash-ref* h (second m)))))))

;;; set operations
(define (hash-minus h1 h2 #:e (e? equal?))
  (let* ((k1s (hash-keys h1))
        (k2s (hash-keys h2)))
    (for/fold
      ((res (hash)))
      ((k1 k1s) (k2 k2s))
      (cond
        ((and (indexof? k2s k1 e?)
              (let* ((k2-pos (indexof k2s k1 e?))
                    (k2 (nth k2s k2-pos)))
                (e? (hash-ref h1 k1) (hash-ref h2 k2))))
          ; if the same element exists in h2, don't include it in the result:
          res)
        (else
          (hash-union
            (hash k1 (hash-ref h1 k1))
            res))))))

(define (all-hash-values h)
  (apply
    append
    (map
      (λ (x) (cond
                ((list? x) x)
                ((scalar? x) (list x))
                (else empty)))
      (hash-values h))))

(define (hash-mask h mask-keys)
  (hash-filter
    (λ (k v) (indexof? mask-keys k))
    h))

(define-catch (deep-hash-mask h mask-keys)
  (for/fold
    ((res (hash)))
    (((k v) h))
    (cond
      ((not (indexof? mask-keys k)) res)
      ((hash? v) (hash-union (hash k (deep-hash-mask v mask-keys)) res))
      (else (hash-union (hash k v) res)))))

(define (break-list-to-hash lst)
  (for/hash ((e lst)) (values e e)))

; (let ((a 10)) (@0 a 'b 100)) -> (hash 'a 10 'b 'b 100 100)
(define-macro (@0 . args)
  `(for/hash ((key ',args) (val (list ,@args)))
    (let ((key (if (symbol-symbol? key) (eval key (make-base-namespace)) key)))
      (values key val))))

(define (hashmap f h)
  (for/hash
    (((k v) h))
    (f k v)))
