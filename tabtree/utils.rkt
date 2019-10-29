#lang racket

;; in this file we represent functions that modify elements of the tree

(require "../lib/_all.rkt")
(require "tab-tree.rkt")

(provide (all-defined-out))

(define (make-index items (index-name "i"))
  (for/fold
    ((res empty))
    ((item items) (idx (in-naturals 1)))
    (pushr
      res
      (hash-union
        (hash index-name idx)
        item))))

; {w,w<r,w>r:first-existed}
(define (first-existed (fallback-value #f))
  (λ args
    (let ((existed (filter-not nil? args)))
      (if (empty? existed)
        fallback-value
        (first existed)))))

(define (add-type val list-of-hashes #:attr (attr 'type))
  (map
    (λ (x) (hash-union (hash attr val) x))
    list-of-hashes))

(define (tabtree-item->string item keys-order)
  (define (parameter->string key params (delimeter ","))
    (cond
      ((list? params) (implode params delimeter))
      ((re-matches? "-f$" (->string key)) (format "`~a`" params)) ; a code snippet
      ((and (string? params) (re-matches? " " params)) (format "\"~a\"" params))
      (else (->string params))))
  (let* ((id ($ id item))
        (keys-to-print (filter-not (λ (x) (let ((x (->string x))) (or (string-prefix? x "_") (equal? x "id")))) (hash-keys item)))
        (keys-order (and keys-order (string-split keys-order ",")))
        (keys-ordered (if keys-order
                            (for/fold
                              ((res empty))
                              ((ko keys-order))
                              (if (indexof*? keys-to-print ko)
                                (pushr res (->symbol ko))
                                res))
                            empty))
        (tail-keys (minus keys-to-print keys-ordered)))
    (for/fold
      ((res (format "~a" id)))
      ((key (append (or keys-ordered empty) (or tail-keys empty))))
      (format "~a ~a:~a" res key (parameter->string key (hash-ref item key))))))

(define (tabtree->string tabtree #:parent (parent-item #f) #:level (level 0) #:keys-order (keys-order #f))
  (let* ((sorted-keys-by-id (sort (hash-keys tabtree) (λ (a b) (< (->number (or ($ _order a) 0)) (->number (or ($ _order b) 0))))))
        (keys-order (or (and parent-item ($ keys-order parent-item)) keys-order))) ; such a way we can define keys-order only in the topmost element and it will be inherited by all lower elements unless they define their own keys-order
    (for/fold
      ((res ""))
      ((k sorted-keys-by-id))
      (begin
        ; (---- sorted-keys-by-id)
        (format "~a~a~a~a"
                (if (or (not-empty-string? res) parent-item) (format "~a~n" res) res) ; don't skip the first line in the file
                (dupstr "\t" level)
                (tabtree-item->string k (or ($ keys-order k) keys-order)) ; first check if current item has keys-order attribute - then apply this one, otherwise use normal keys-order from the parent
                (tabtree->string (hash-ref tabtree k) #:parent k #:level (+ 1 level) #:keys-order keys-order))))) )

; hashtree -> list-of hash
(define (get-first-level hashtree)
  (hash-keys hashtree))

; hashtree -> hashtree
(define (get-level-below hashtree)
  (let* ((root-element (get-root-item hashtree)))
    (hash-ref hashtree root-element #f)))

(define (default-sorter a b)
  (a-z ($ id a) ($ id b)))

(define default-sort-f a-z)
(define default-sort-by 'id)

(define (hash-id h)
  ($ id h))

; Important! Tabtree file should contain only one root element. Other first level elements and their chikdren will be deleted after sorting.
(define-catch (tabtree-sort-and-print treefile #:ns (ns #f) #:new-treefile (new-treefile #f) #:sort-by (sort-by default-sort-by) #:sort-f (sort-f default-sort-f))
  (define-catch (tabtree-sort-rec root-item root-hashtree #:sort-by sort-by #:sort-f sort-f)
    (cond
      ((not (hash? root-hashtree)) root-hashtree)
      ((hash-empty? root-hashtree) root-hashtree) ; empty list
      ((not-hashes? (hash-keys root-hashtree)) root-hashtree) ; end element
      (else
        (let* (
              (embedded-sort-by (and ($ sort-by root-item) ns (read (open-input-string ($ sort-by root-item) ns))))
              (embedded-sort-by (and embedded-sort-by (->symbol embedded-sort-by)))
              (embedded-sort-f (and ($ sort-f root-item) ns (read (open-input-string ($ sort-f root-item) ns))))
              (embedded-sort-f (and embedded-sort-f (->symbol embedded-sort-f)))
              (sort-by (or embedded-sort-by sort-by))
              (sort-f (or embedded-sort-f sort-f))
              (root-hashtree-keys (hash-keys root-hashtree))
              (sorter (λ (a b) (let ((a-val (hash-ref a sort-by #f))
                                    (b-val (hash-ref b sort-by #f)))
                                  (cond
                                    ((and a-val b-val)
                                      ((eval sort-f ns) a-val b-val))
                                    (else
                                      (let ((a-val (hash-ref a default-sort-by #f))
                                            (b-val (hash-ref b default-sort-by #f)))
                                      (default-sort-f a-val b-val))))))))
            (for/hash
              ((k (sort root-hashtree-keys sorter)) (i (range 1 (inc (length root-hashtree-keys)))))
              (values (hash-union (hash '_order i) k) (tabtree-sort-rec k (hash-ref root-hashtree k) #:sort-by sort-by #:sort-f sort-f)))))))
  (let* ((hashtree (parse-tab-tree treefile))
        ; (_ (--- hashtree))
        (root-item (get-root-item hashtree))
        (hashtree-sorted (hash
                            root-item
                            (tabtree-sort-rec root-item (get-level-below hashtree) #:sort-by sort-by #:sort-f sort-f)))
        (new-treefile-name (if new-treefile
                              new-treefile
                              (let*
                                  ((filename-parts (string-split treefile #rx"(?<=[A-Za-zА-Яа-я0-9_])\\.(?=[A-Za-zА-Яа-я0-9_])"))
                                  (aname (car filename-parts))
                                  (ext (and (not-empty? (cdr filename-parts)) (cadr filename-parts))))
                                (str aname "_" (if ext (str "." ext) "")))))
        (new-treefile-str (tabtree->string hashtree-sorted)))
      (write-file new-treefile-name new-treefile-str)
      #t))

(define (set-current-page-mark id curpage-id (class "current_page_nav"))
  (if (equal? (->string id) (->string curpage-id))
    (format "class=\"~a\"" class)
    ""))

(define (tabtree-true? v)
  (cond
    ((equal? (->string v) "<t>") #t)
    ((equal? (->string v) "<f>") #f)
    (else #t)))
