#lang racket

(require compatibility/defmacro)
(require "text.rkt")
(require "main.rkt")

(provide (all-defined-out))

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

(define textify (change-text
                  (list
                    (cons " - " " &ndash; ")
                    (cons "'" "\"")
                    (cons #px"(?<=\\S),(?=\\S)" ", "))))

(define linefy (change-text
                  (list
                    (cons "\t" " ")
                    (cons "\n" " ")
                    (cons "\r" ""))))

(define clean-newlines linefy)

(define (httpify txt (prefix "http"))
  (cond
    ((not txt) txt)
    ((empty-string? txt) txt)
    ((re-matches? "^https?://" txt) txt)
    ((re-matches? "^\\./" txt) txt)
    (else (str prefix "://" txt))))

(define (make-link id #:target (target "_blank") #:anchor (anchor #f) urls)
  (let* ((name (namefy id)))
    (if (and (andmap nil? urls) (not anchor))
      name
      (format "<a id=\"~a\" href=\"~a\" target=\"~a\">~a</a>"
              id
              (if anchor
                (format "#~a" (if (equal? anchor "#") id anchor))
                (httpify (for/or ((url urls)) url)))
              target
              name))))

(define htmlify-text
  (change-text
    (list
      ; add line breaks
      (cons "\r\n" "<br>")
      (cons "\n" "<br>")
      (cons "\"" "&quot;")
    )))

(define clean-htmlify (compose clean-text htmlify-text))

(define clean-value
          (change-text
            (list
              (cons "\"" " ")
              (cons "&nbsp;" " ")
              ; (cons "," "")
              (cons "\n" " ")
              (cons "\t" "")
              (cons "  " " ")
              (cons " ." ".")
              (cons "<span>" "")
              (cons "</span>" ""))))

; used for comparing messages in vk
(define simplify-text
  (change-text
    (list
      (cons "." " ")
      (cons "—" "-")
      (cons "\n" " ")
      (cons #px" $" "")
      (cons #px"\\s*-\\s*" "-")
      (cons #px"\\s{2,}" " ")
      (cons #px"[\\.,;:\\\\/\\!?()\\[\\]]" ""))))
