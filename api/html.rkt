#lang racket

(require compatibility/defmacro)
(require "../main.rkt")

(provide (all-defined-out))

(define ADVANCED-HTML-TEMPLATE
  (str
    "<!DOCTYPE html>"
    "<html>"
    "  <head>"
    "    <meta charset=\"utf-8\">"
    "    <title>~a</title>"
    "    <style>"
    "      * { font-family: Arial, sans-serif; line-height: 1.5em;}"
    "      h1 { font-size: 160%; }"
    "    </style>"
    "  </head>"
    "  <body>"
    "    <!-- title: -->"
    "    <h1>~a</h1>"
    "    <!-- lead: -->"
    "    <p>~a</p>"
    "    <!-- body: -->"
    "    ~a"
    "  </body>"
    "</html>"       ))

(define (write-html-file filename title body #:lead (lead ""))
  (let* ( (html-format ADVANCED-HTML-TEMPLATE)
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

(define (html #:title (title "") #:head (head "") #:css (css "") #:js (js "") #:html (html-content ""))
  (let ((frmt
            (implode
                  (list
                    "<!DOCTYPE html>"
                    "<html lang=\"en\">"
                    " <head>"
                    "   <meta charset=\"utf-8\">"
                    "   <title>~a</title>"
                    "   <link rel=\"stylesheet\" href=\"~a\">"
                    "   <script src=\"~a\"></script>"
                    "   ~a"
                    " </head>"
                    " <body>"
                    "   ~a"
                    " </body>"
                    "</html>"
                  "~n"))))
    (format
      frmt
      title css js head html-content)))

; wrapper for md format
(define (md #:title (title "") content)
  (let ((frmt
            "# ~a~n\
            ~n~n\
            ~a"))
  (format
    frmt
    title content)))
