#lang racket

(require "../utils/base.rkt")
(require "../utils/hash.rkt")
(require "../widgets/styles.rkt")

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

; TODO text-struct
(define-struct text: (text font-size font-family font-style) #:mutable)

(define (text-length t)
  (let* ([txt (if (text:? t) t (text: t (@base-property 'font-size) (@base-property 'font-family) (@base-property 'font-style)))]
        [res (* (text:-font-size txt)
                (@font-aspect-ratio (text:-font-family txt) (text:-font-style txt))
                (string-length (text:-text txt)))])
    ; (println res)
    res))

(define (text-height t)
  (if (text:? t)
    (text:-font-size t)
    (@base-property 'font-size)))

(define (centrify left-edge right-edge $txt)
  (let ([w (- right-edge left-edge)]
        [tl (text-length $txt)])
    (/r (- w tl) 2)))
