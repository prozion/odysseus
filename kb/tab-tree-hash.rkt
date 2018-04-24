#lang racket

(require "../lib/load/all.rkt")

(provide (all-defined-out))

;;; These functions onvert tree format to a list of hashes,
;;; where each parameter in tree format becomes a key in the hash structure

(define-catch (line->hash line)
  (let* ((res-name (get-matches (format "^(~a)*([^:\\s]+)(\\s|$)" "\t") line))
        (_name (if (empty? res-name) #f (nth (nth res-name 1) 3)))
        (res-string-parameters
              (get-matches (format "(\\S+):\"(.+?)\"") line))
        (res-parameters
              (get-matches (format "(\\S+):(\\S+)") line))
				(parameters (for/hash ((p res-parameters)) (values (->symbol (list-ref p 1)) (->symbol (list-ref p 2)))))
				(string-parameters (for/hash ((p res-string-parameters)) (values (->symbol (list-ref p 1)) (list-ref p 2))))
        (hash-res (hash-union string-parameters parameters)))
    (if _name (->symbol _name) hash-res)))

; (define-catch (add-item-to-hash-tree tree item path)
;   (hash-

(define-catch (add-item-to-tree tree item level)
  (cond
    ((= level 0) (pushr tree item))
    ((not (list? (last tree))) (pushr tree (list item)))
    (else
      (pushr (rtrim tree 1) (add-item-to-tree (last tree) item (dec level))))))

(define-catch (fill-tree-iter source-lines result-tree)
  (cond
    ((empty? source-lines) result-tree)
    (else
      (let* ((line (car source-lines))
            (level (count-tabs line))
            (line-hash (line->hash line))
            (new-result-tree (add-item-to-tree result-tree line-hash level)))
        (fill-tree-iter (cdr source-lines) new-result-tree)))))

(define-catch (parse-tab-tree-hash filename)
  (let* (
        (lines (read-file-by-lines filename))
        (lines (clean
                      (λ (line) (or
                                  (re-matches? "^\t*;" line)
                                  (re-matches? "^\\s*$" line)))
                      lines)))
    (tree->hash
      (fill-tree-iter lines (list)))))
