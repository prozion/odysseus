#lang racket

(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-defined-out))

(define (write-html-file filename title body #:lead (lead ""))
  (let* ( (html-format (read-file (string-append (getenv "odysseus") "/templates/advanced.fhtml")))
          (body (cond
                  ((string? body) body)
                  ((list2? body) (div-iter body ""))
                  ((list? body) (format "<ul>~a</ul>" (li-iter body "")))
                  (else "unknown format")))
          (res (format html-format title title lead body)))
  (display "\nready!")
  (write-file filename res)))

(define (div-iter body res)
  (cond
    ((empty? body) res)
    (else (div-iter
            (cdr body)
            (format
              "~a~n<div>~a</div>~n"
              res
              (let ((item (car body)))
                (cond
                  ((list? item) (implode item " – "))
                  (else (str item)))))))))

(define (li-iter body res)
  (cond
    ((empty? body) res)
    (else (li-iter (cdr body) (format "~a~n<li>~a</li>~n" res (str (car body)))))))


  (define (html-color s color)
    (format "<span style='color: ~a'>~a</span>" color s))

  (define (html-a href text #:color (color null))
    (let ((style (if (or color)
                    (format "style=\"color: ~a\"" color)
                    "")))
      (format "<a href=\"~a\" ~a>~a</a>" href style text)))
