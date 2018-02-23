#lang racket

(require compatibility/defmacro)
(require "../../graphics/svg.rkt")
(require "../../lib/load/all.rkt")

(provide (all-defined-out))

; (heatmap '(1 2 1 3 4 2 4) #:translation (@ 1 "red" 2 "green" 3 "yellow" 4 "blue") #:width 3 #:gap 1)
(define (heatmap
            #:data (data #f)
            #:datafile (datafile #f)
            #:chunk-size (chunk-size #f)
            #:generalization (generalization-lambda #f)
            #:translation (translation-table identity)
            #:width (width 0)
            #:gap (gap 0)
            #:size-w (size-w 10)
            #:size-h (size-h 10)
            #:novalue (novalue "black"))
  (let*
    ((data (if datafile
              (filter
                (λ (x) (indexof? "atgc" (string-downcase x)))
                (explode (read-file datafile)))
              data))
    (_ (__t "filtered\n"))
    (data (if (and generalization-lambda chunk-size)
                (map generalization-lambda (partition data chunk-size))
                data))
    (_ (__t "generalized\n"))
    (translated-data (map
                        (cond
                          ((hash? translation-table) (λ (x) (hash-ref translation-table x novalue)))
                          ((procedure? translation-table) translation-table)
                          (else (λ (x) novalue)))
                        data))
    (_ (__t "translated\n"))
    (chunked-data (partition-all translated-data width))
    (_ (__t "partitioned\n"))
 )
    (for/fold
      ((s ""))
      (
        (chunk chunked-data)
        (j (range 0 (length chunked-data))))
        (_t ".")
        (str
          s
          (for/fold
            ((s ""))
            (
              (d chunk)
              (i (range 0 (length chunk))))
            (str
              s
              (rect
                x (* i (+ size-w gap))
                y (* j (+ size-h gap))
                width size-w
                height size-h
                style (format "fill:~a" d))))))))
