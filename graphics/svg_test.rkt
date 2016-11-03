#lang racket

; TODO:
; + fix 'find end-of-file error' when running 'raco test spec.rkt'
; - make rand-color function as short as original one in newlisp
; - related to previous: learn analogs for reduce, and other javascript functions that were handy in working with arrays and strings, if no direct analog, and no handy substitution - implement this function in utils.rkt

(module+ test

  (require rackunit)
  (require "svg.rkt")

  ; SVG
  ; (svg
    ;(g
      ;(rect #:x 10 #:y 10 #:width 100 #:height 150 #:fill "red")
      ;(rect #:x 10 #:y 10 #:width 100 #:height 150 #:fill "red")))

  (check-equal?
    (svg)
    #<<svg
<svg></svg>
svg
  )

  (check-equal?
    (svg #:xmlns #t)
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg"></svg>
svg
  )

  (check-equal?
    (svg #:xmlns #t #:xlink #t)
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>
svg
  )

  (check-equal?
    (svg
      (g
        (rect #:x 10 #:y 10 #:width 100 #:height 150 #:fill "red")
        (rect #:x 100 #:y 400 #:width 200 #:height 300 #:fill "cyan")))
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg">
<g>
<rect x='10' y='10' width='100' height='150' fill='red' />
<rect x='100' y='400' width='200' height='300' fill='cyan' />
</g>
</svg>
svg
  )

  ; G
  ; (g #:id "external" (g #:id "inner" (rect #:x 10 #:y 10 #:width 100 #:height 150 #:fill "red")))


  (check-equal?
    (g "hello world! "
      (rect #:x 10
            #:y 10
            #:width 100
            #:height 150
            #:fill "red"))
    "<g>hello world! <rect x='10' y='10' width='100' height='150' fill='red' />\r\n</g>\r\n")


  ; RECT
  ; (rect #:x 10 #:y 10 #:width 100 #:height 150 #:fill "red")

  (check-equal?
    (rect #:width 100
          #:x 10
          #:y 10
          #:height 150)
    "<rect x=\"10\" y=\"10\" width=\"100\" height=\"150\" fill=\"black\" />\r\n")
  (check-equal?
    (rect #:x (* (sqrt 4) 100)
          #:y 10
          #:width 100
          #:height 150
          #:fill "red")
    "<rect x=\\"200\\" y=\\"10\\" width=\\"100\\" height=\\"150\\" fill=\\"red\\" />\r\n")

;  ;; ANY TAG, ANY ATTRIBUTE (how to use racket functions then?)
;  (check-equal?
;    (foobar #:foo 10 #:bar "hello"
;      (fooqux #:id "qux" #:quux 100)
;      (foobaz
;        (fooquux #:id "fooquux" "hello world!")
;        #:id "foobaz_id" #:pid 3300)
;      (fooquxbaz "Autumn!"))
;#<<svg
;<foobar foo="10" bar="hello">
;<fooqux id="qux" quux="100" />
;<foobaz id="foobaz_id" pid="3300">
;<fooquux id="fooquux">hello world!<fooquux>
;</foobaz>
;<fooquxbaz>Autumn!</fooquxbaz>
;</foobar>
;svg
;  )

)
