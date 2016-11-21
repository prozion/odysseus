#lang racket

(require racket/file)
(require (for-syntax racket/syntax))
(require "../utils/base.rkt" (for-syntax "../utils/base.rkt")) ;; STX for-syntax ~require
(require "../utils/hash.rkt")
(require "../utils/io.rkt")
(require "fonts.rkt")
(require "../utils/seqs.rkt" (for-syntax "../utils/seqs.rkt"))
(require compatibility/defmacro)
(require racket/runtime-path)

(define-runtime-path rootpath "..")

(provide (all-defined-out))

(define (foo) "bar")

(define-macro (svg . args)
  (let* ( (xmlns (if (indexof? args 'xmlns) #t #f))
          (xlink (if (indexof? args 'xlink) #t #f))
          (styles (if (indexof? args 'styles) #t #f))
          (body (clean (λ (x) (or (equal? x 'xmlns) (equal? x 'xlink)  (equal? x 'styles) )) args))
          (body (if (null? body) "" (car body))))
    `(svg-f ,xmlns ,xlink ,styles ,body)))

(define (svg-f
          xmlns
          xlink
          styles
          . body)
  (let ([xmlns (if xmlns
                      " xmlns=\"http://www.w3.org/2000/svg\""
                      "")]
        [xlink (if xlink
                      " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
                      "")]
        [styles (if styles
                      (str  "\n<style type=\"text/css\">\n"
                            "/* <![CDATA[ */\n"
                            (read-file (str rootpath "\\templates\\styles.css"))
                            "/* ]]> */\n"
                            "</style>\n")
                      "")]
        [body (if (empty? body) "" (apply string-append body))])

    (format "<svg~a~a>~a~a</svg>" xmlns xlink styles body)))

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
(make-tag text)
