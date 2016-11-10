#lang racket

(require racket/file)
(require (for-syntax racket/syntax))
(require "../utils/base.rkt" (for-syntax "../utils/base.rkt"))
(require "../utils/hash.rkt")
(require "../utils/seqs.rkt" (for-syntax "../utils/seqs.rkt"))
(require compatibility/defmacro)

(provide (all-defined-out))

;; STX define-syntax-rule, syntax-case, with-syntax as constructions
;; STX syntax->list, syntax->datum etc. as standard functions

(define @ hash)

(define (svg
          (attrs (hash 'xmlns #f 'xlink #f))
          . body)
  (let ([xmlns (if (hash-ref attrs 'xmlns #f) " xmlns=\"http://www.w3.org/2000/svg\"" "")]
        [xlink (if (hash-ref attrs 'xlink #f) " xmlns:xlink=\"http://www.w3.org/1999/xlink\"" "")]
        [body (if (empty? body) "" (apply string-append body))])
    (format "<svg~a~a>~a</svg>" xmlns xlink body)))


; e.g.: (rect x 10 y 10 width 100 height 100) as well as (rect 'x 10 'y 10 'width 100 'height 100)
(define-macro (make-single-tag tagname)
  `(define-macro (,tagname . body)
    (define (odd-f f lst)
      (cond
        ((null? lst) null)
        ((null? (cadr lst)) (cdr lst))
        (else (cons (f (car lst)) (cons (cadr lst) (odd-f f (cddr lst)))))))
    (let ([nbody (odd-f (λ(x) (if (symbol? x) (symbol->string x) x)) body)]
          [t (symbol->string (quote ,tagname))])
      `(str "<" ,t (print-hash " ~a=\"~a\"" (hash ,@nbody)) " />"))))

(define-syntax (make-tag stx)
  (let ((tagname (symbol->string (list-ref (syntax->datum stx) 1))))
    (datum->syntax
      stx
      `(begin
          (define (,(string->symbol (string-append tagname "1"))
                    (attrs (hash)) . body)
                      (string-append
                        ,(string-append "<" tagname)
                        (if ((hash-length attrs) . > . 0) (print-hash " ~a=\"~a\"" attrs) "")
                        ">"
                        (apply str (if (empty? body) empty body))
                        ,(string-append "</" tagname ">")))
          (define (,(string->symbol (string-append tagname "2"))
                    . body)
                      (string-append
                        ,(string-append "<" tagname ">")
                        (apply str (if (empty? body) empty body))
                        ,(string-append "</" tagname ">")))
          (define-syntax (,(string->symbol tagname) stx)
                      (let (( tl (syntax->datum stx)))
                        (if ((length tl) . < . 2)
                          (datum->syntax stx (string-append "<" ,tagname "></" ,tagname ">"))
                          (let* ( (arg1 (if (list? (cadr tl)) (caadr tl) null))
                                  (args (cdr tl))
                                  (,(string->symbol (string-append tagname "1")) (datum->syntax stx (cons ',(string->symbol (string-append tagname "1")) args)))
                                  (,(string->symbol (string-append tagname "2")) (datum->syntax stx (cons ',(string->symbol (string-append tagname "2")) args))))
                            ;(printf "tagname1: ~a~n" ,(string->symbol (string-append tagname "1")))
                            (if (equal? arg1 '@)
                              ,(string->symbol (string-append tagname "1"))
                              ,(string->symbol (string-append tagname "2")))))))))))

(make-single-tag rect)
(make-tag g)
