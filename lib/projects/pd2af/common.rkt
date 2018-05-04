#lang racket

(require "../../load/all.rkt")

(provide (all-defined-out))

(define defaults (hash
                    'W 1000 'H 800
                    'el-w 110 'el-h 50
                    'sink-w 25 'sink-h 25
                    'gate-w 25 'gate-h 25 'gate-margin 5
                    'process-w 15 'process-h 15
                    'compartment-margin-x 30 'compartment-margin-y 30
                    'square-w 200 'square-h 100 ; side sizes of a q-grid
                    'default-x 0 'default-y 0 ; x,y of element if not any hints (q parameter, location in the complex etc.)
                    'component-vertical-gap 15
                    'uoi-w 40 'uoi-h 10
                    'state-variable-positions '(70 10 40 100)
                    'default-state-variables '(p P state)
                    'state-variable-d 15 'state-variable-w 30 'state-variable-h 15
                    ))


(define (->id id)
  (if (re-matches? "\\d.*" (->string id))
    (->symbol (str "id" (->string id)))
    (->symbol id)))

(define (get-element-by-id id context (debug #f))
  (let ((matched-elements
          (filter
            (λ (el) (equal? ($ id el) id))
            context)))
    (if (empty? matched-elements)
      #f
      (car matched-elements))))

(define ($$->el id context (keys #f))
  (cond
    ((or (not keys) (empty? keys))
      (get-element-by-id id context))
    ((scalar? keys)
      (let ((res (filter
                    (λ (el) (equal? (hash-ref el keys #f) id))
                    context)))
        (if (empty? res) #f (car res))))
    ((list? keys)
      (or ($$->el id context (car keys)) ($$->el id context (cdr keys))))
    (else #f)))

(define (get-pd-filename path (ext ".pd.sbgn"))
  (let* (
  				(filename (string-replace path "\\" "/"))
  				(filename (string-split filename "/"))
  				(filename (last filename))
  				(filename (string-split filename "."))
  				(filename (first filename))
  				(filename (str filename ext)))
    filename))

(define (class-name class)
  class)
  ; (->symbol (string-replace (->string class) " " "_")))

(define (id-prefix id)
  (format "~a-" (string-replace (->string id) " " "-")))

(define context-element? hash?)

; orders sxml lists by tagname
(define (order-by-tag tags-order sxml)
	(sort sxml
		(λ (a b)
      (and (list? a) (list? b)
  			(let* ((tag-a (car a))
  						(tag-b (car b))
  						(pos-a (indexof tags-order tag-a))
  						(pos-b (indexof tags-order tag-b)))
  				(< pos-a pos-b))))))

(define-catch (ontology-uoi? uoi)
	(and uoi
			(re-matches? ":|ct:|mt:" uoi)))

(define-catch (get-node-name el)
	(format "~a~a"
					($ name el)
          ""))
					; (if (and ($ uoi el) (non-ontology-uoi? ($ uoi el)))
					; 	(str "-" ($ uoi el))
					; 	"")))
