#lang racket

(require "widgets/barchart.rkt" "widgets/heatmap.rkt")
(require "../graphics/svg.rkt")
(require compatibility/defmacro)
(require "../lib/all.rkt")

(provide (all-from-out "widgets/barchart.rkt" "widgets/heatmap.rkt") (all-defined-out))

(define widgets str)

(define (raster-wrapper params svg-str filetype)
  (define (save-raster #:file file #:dir dir)
    (let ([w (hash-ref params 'w 100)]
          [h (hash-ref params 'h 100)])
      ;; - save content as temp.svg
      (write-file "temp.svg" (svg (@ 'viewbox (list 0 0 w h)) xmlns xlink styles svg-str))

      ;; - convert temp.svg dir/file
      (system (format "convert -size ~ax~a temp.svg ~a" w h (str dir file)))
      ; (shell-execute  #f (format "convert -size ~ax~a temp.svg temp.png" w h) "" (current-directory) 'SW_SHOWDEFAULT)))

      ;; - delete temp.svg
      (delete-file "temp.svg")))

  (@ 'output-file-ext filetype 'save-file save-raster))

(define (.png params svg-str)
  (raster-wrapper params svg-str ".png"))

(define (.svg svg-str)
  (define (save-svg #:file file #:dir dir)
    (write-file-to-dir
      #:file file
      #:dir dir
      (svg xmlns xlink styles
        svg-str)))
  (@ 'output-file-ext ".svg" 'save-file save-svg))
