#lang racket

(require "../lib/load/all.rkt")
(require sxml)

(provide (all-defined-out))

; (hash 'a 10 'b 20) -> "a:10, b:20"
(define (style->string style)
  (for/fold
    ((res ""))
    ((key (hash-keys style)))
    (str
      res
      (if (> (string-length res) 0) "; " "")
      (format "~a:~a" (->string key) (hash-ref style key)))))

(define (svg sxml (width "100%") (height "100%"))
  (format "~a~n~a~n~a~n<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"~a\" height=\"~a\">~n~a~n</svg>"
            "<?xml version=\"1.1\" standalone=\"no\"?>"
            "<?xml-stylesheet type=\"text/css\" href=\"styles.css\"?>"
            "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
            width height
            (srl:sxml->xml sxml)))
  ; (srl:sxml->xml
  ;   (format-list
  ;     '(svg (@ (xmlns "http://www.w3.org/2000/svg")) ~a)
  ;     sxml)))

(define (write-svg filename sxml #:width (width #f) #:height (height #f))
  (write-file
    filename
    (apply svg (cleanmap (list sxml width height)))))
