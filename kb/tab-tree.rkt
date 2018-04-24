#lang racket

(require "../lib/load/all.rkt")

(provide (all-defined-out))

;;;;; For storing data from tabtree format we will use a following model:
;;;;; (hash a (hash) b (hash) c (hash c1 (hash) c2 (hash c21 (hash))))
;;;;; We build a nested hash structure (hashtree), where every element is a hash key.
;;;;; For manipulation with hashtrees we use functions from hashtree.rkt

(define-catch (get-item-name item)
  (let ((name (hash-ref item 'name #f))
        (id (hash-ref item key-name #f)))
    (cond
      (name #f)
      ((hash-empty? item) #f)
      (else (string-replace (->string id) "_" " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Reads one line from a tab-tree file into a hash.
;;; Line has a following format:
;;; <name> <parameter-name>:<parameter-value> ... <string-parameter-name>:"<string-parameter-value>" ...
;;;
;;; The resulting hash is (hash <key-name> <name> <parameter-name> <parameter-value> ... <string-parameter-name> "<string-parameter-value>" ...)
;;; This hash is identified by <key-name> special attribute. Underscore in the beginning of key name is reserved symbol in this system of data representation.
;;; All keys that starts with _ has a special meaning.
(define-catch (get-item line #:symbol (symbol "\t"))
  (let* (
        ; read <name>
        (res-name (get-matches (format "^(~a)*(\\S+)" symbol) line))
        ; read all parameters, that have a string value
        (res-string-parameters
              (get-matches (format "(\\S+):\"(.+?)\"") line))
        ; read all parameters, that have value as reference, date, number, special value etc. except string
        (res-parameters
              (get-matches (format "(\\S+):(\\S+)") line))
        ; accumulate parameters to a hash
				(parameters (for/hash ((p res-parameters)) (values (->symbol (list-ref p 1)) (->symbol (list-ref p 2)))))
        ; accumulate string parameters to a hash
				(string-parameters (for/hash ((p res-string-parameters)) (values (->symbol (list-ref p 1)) (list-ref p 2))))
        ; join two hashes
        (res (hash-union string-parameters parameters))
        ; add id to item
        (res (if res-name
            (hash-insert res (cons key-name (nth (nth res-name 1) 3)))
            res))
        ; add name to item, if it not already defined:
        (name (get-item-name res))
        (res (if name
                  (hash-insert res (cons 'name name))
                  res)))
    res))

; Iteratively populates nested list structure (tree) with cons-hashes (item)
(define-catch (fill-tree-iter source-lines result-tree curpath)
  (cond
    ; if no more lines then exit the iteration and return result-tree
    ((empty? source-lines) result-tree)
    ; if some more source-lines...
    (else
      (let* ((line (car source-lines)) ; take the next source-line, now it is a "current line"
            (level (count-tabs line))
            (delta-level (- level (dec (length curpath)))) ; find the level of the current line
            (item (get-item line)) ; parse current line to item
            ; convert to string all general (not special) parameters:
            (item (hash-map
                    (λ (k v) (if (special-parameter? k)
                                (values k v)
                                (values k (->string v))))
                    item))
            ; direct order (a1 a2 a3), reverse order (a3 a2 a1)
            ; parents - reverse order
            (parents (cond
                        ((> delta-level 0)
                            curpath) ; take current last element in the path as parent -> go down one level
                        ((= delta-level 0)
                            (cdr curpath)) ; take the parent of last element as a parent, last element becomes a neighbour to the new item
                        ((< delta-level 0)
                            (triml curpath (+ 1 (abs delta-level))))))
            (nextpath (cons (get-key item) parents))
            (item (if (empty? parents)
                      (add-parent item #f) ; root element has no parents
                      (add-parent item (car parents)))) ; otherwise, a parent is the last element in the path sequence
            ; add label to item
            (item (hash-union (hash '_label (if (empty? curpath) "" (last curpath))) item))
            ; add item to the result-tree:
            (new-result-tree (cond
                                ; if empty hash, then don't do anything with result-tree
                                ((hash-empty? item) result-tree)
                                ; otherwise add item to the end of parents sequence
                                (else (hash-tree-add-value-by-id-path* result-tree (reverse parents) item key-name)))))
        (fill-tree-iter (cdr source-lines) new-result-tree nextpath))))) ; repeat the process for the rest of source lines

; Initialize population of a tree with data from tabtree file
(define-catch (parse-tab-tree tree-file)
  (let* (
        ;; take all lines from the file:
        (tree-lines (read-file-by-lines tree-file))
        ;; remove comment lines:
        (tree-lines (clean
                      (λ (line) (or
                                  (re-matches? "^\t*;" line)
                                  (re-matches? "^\\s*$" line)))
                      tree-lines)))
      ;; start populating the tree
      (fill-tree-iter tree-lines (hash) empty)))
