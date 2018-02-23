#lang racket

(require "../lib/load/all.rkt")

(provide text-length text-height h-centrify v-centrify)

(define FONT_SIZE 10)
(define FONT_FAMILY "Arial")
(define FONT_STYLE "normal")

; some info from here: http://webdesign.about.com/od/fonts/a/font-aspect-ratio-table.htm
(define font-aspect-ratios
            (@
              "Arial" (@ "normal" 0.52 "bold" 0.6)
              "Avant Garde" (@ "normal" 0.45)
              "Bebas Neue Bold" (@ "normal" 0.8)
              "Bookman" (@ "normal" 0.4)
              "Calibri" (@ "normal" 0.47)
              "Century Schoolbook" (@ "normal" 0.48)
              "Cochin" (@ "normal" 0.41)
              "Comic Sans" (@ "normal" 0.53)
              "Courier" (@ "normal" 0.43)
              "Courier New" (@ "normal" 0.42)
              "Franklin Gothic Heavy" (@ "normal" 0.6)
              "Garamond" (@ "normal" 0.38)
              "Georgia" (@ "normal" 0.48)
              "Helvetica" (@ "normal" 0.52)
              "Palatino" (@ "normal" 0.42)
              "Tahoma" (@ "normal" 0.55 "bold" 0.7)
              "Times New Roman" (@ "normal" 0.422 "bold" 0.45)
              "Trebuchet" (@ "normal" 0.52)
              "Open Sans" (@ "normal" 0.5)
              "Verdana" (@ "normal" 0.55)
         )
)

(define (get-font-aspect-ratio . path)
  (apply (curry hash-path font-aspect-ratios) path))

(define (text-length  text
                      #:font-size (font-size FONT_SIZE)
                      #:font-family (font-family FONT_FAMILY)
                      #:font-style (font-style FONT_STYLE))
    (*  font-size
        (get-font-aspect-ratio font-family font-style)
        (string-length text)))

(define (text-height t #:font-size (font-size FONT_SIZE))
  font-size)

(define (h-centrify w t #:font-family (font-family FONT_FAMILY) #:font-size (font-size #f))
  (let* ( (t (if (hash? t) (hash-ref t 'text "") t))
          (font-size (if font-size
                        font-size
                        (if (hash? t)
                          (hash-ref t 'font-size FONT_SIZE)
                          FONT_SIZE)))
          [tl (text-length t #:font-size font-size #:font-family font-family)])
    (/r (- w tl) 2)))

(define (v-centrify h #:font-size (font-size FONT_SIZE))
  (+ (/r h 2) (/r font-size 2)))
