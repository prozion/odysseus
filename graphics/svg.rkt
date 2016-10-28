#lang at-exp racket

(provide (all-defined-out))

(define (svg . args)
;; TODO: define contract for args (avoid empty lists)
  (let ((body args))
    @string-append{<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">\r\n@(if (empty? body) "" (apply string-append body))\r\n</svg>}
    ))

(define (g . body)
  (string-append "<g>"
    (apply string-append (if (empty? body) empty body))
    "</g>\r\n"))

(define (rect #:x x
              #:y y
              #:width width
              #:height height
              #:fill (fill "black"))
  (format "<rect x='~s' y='~s' width='~s' height='~s' fill='~a' />\r\n" x y width height fill))
