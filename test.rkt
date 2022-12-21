#lang racket

(require compatibility/defmacro)
(require "debug.rkt")
(require "seqs.rkt")

(provide (all-defined-out))

(define SPEED-UNIT (benchmark (apply / (range 1 2000)))) ; ~15 ms

(define-macro (check-speed expr high-limit)
  `(if (> (benchmark ,expr) (* ,high-limit SPEED-UNIT))
    (printf "~a - running time exceeds ~a speed units~n" ',expr ,high-limit)
    (void)))
