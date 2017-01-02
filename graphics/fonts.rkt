#lang racket

(require "../lib/base.rkt")
(require "../lib/hash.rkt")
(require "../reports/styles.rkt")

(provide (all-defined-out))

; some info from here: http://webdesign.about.com/od/fonts/a/font-aspect-ratio-table.htm
(define @font-aspect-ratios
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
              "Tahoma" (@ "normal" 0.55)
              "Times New Roman" (@ "normal" 0.422 "bold" 0.45)
              "Trebuchet" (@ "normal" 0.52)
              "Open Sans" (@ "normal" 0.5)
              "Verdana" (@ "normal" 0.55)
            )
)

(define (@font-aspect-ratio . path)
  (apply (curry hash-path @font-aspect-ratios) path))

(define (text-length t)
  (let* (
          [t (if (hash? t) t (hash 'text t))]
          [text (hash-ref t 'text "")]
          [font-size (hash-ref t 'font-size (@base-property 'font-size))]
          [font-family (hash-ref t 'font-family (@base-property 'font-family))]
          [font-style (hash-ref t 'font-style (@base-property 'font-style))])
    (*  font-size
        (@font-aspect-ratio font-family font-style)
        (string-length text))))

(define (text-height t)
  (if (hash? t)
    (hash-path t 'font-size (@base-property 'font-size))
    (@base-property 'font-size)))

(define (h-centrify w t)
  (let ([tl (text-length t)])
    (/r (- w tl) 2)))

(define (v-centrify h (font-size (@base-property 'font-size)))
  (+ (/r h 2) (/r font-size 2)))
