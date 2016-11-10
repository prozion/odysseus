#lang racket

;; TODO: (require "../utils/all.rkt") instead of requiring the files one by one
(require "../graphics/svg.rkt")
(require "../utils/seqs.rkt")
(require "../utils/base.rkt")
(require "../utils/iter.rkt")
(require "../utils/hash.rkt")

(provide (all-defined-out))

; data ~ '(10 2 -3 46 8)
; (barchart '(3 4 8 1 9 5))
(define (barchart data (box '(800 600)) (gap 1) (color "black") (normalize-mode 'trunc-at-zero))
  (define (normalize data factor)
    (map factor
      (case normalize-mode
        ([min-to-zero]
          (let* (
                [data-min (apply min data)]
                [n-data (if (< data-min 0)
                                    (map (λ (x) (+ x (abs data-min))) data)
                                    (map (λ (x) (- x data-min)) data) )])
            n-data))
        ([trunc-at-zero]
          (map (λ (x) (if (< x 0) 0 x)) data))
        (else data))))
  (let* (
        [top-margin 50]
        [box-w (nth box 1)]
        [box-h (nth box 2)]
        [amount (length data)]
        [bar-w ( { box-w . - . [ amount . * . gap ] } . /r . amount )]
        [base (+ box-h top-margin)]
        [data-max (apply max data)]
        [factor (λ (x) (/ (* x box-h) data-max))])
    (g
      (for/fold/idx
        (s "")
        (h (normalize data factor))
          (str s
              (rect x (* $idx (+ bar-w gap))
                y (- base h)
                width bar-w
                height h))))))
