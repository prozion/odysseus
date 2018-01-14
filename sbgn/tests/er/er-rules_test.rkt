#lang racket

(module+ test

  (require rackunit)
  (require "../../sbgn/er-rules.rkt")

  (check-equal?
    (get-new-state 'exist 'catalyzed)
    'catalyzed)

  (check-equal?
    (get-new-state 'foo 'catalyzed)
    #f)

  (check-equal?
    (get-synapse-signal 'blocked 'absolute-inhibition)
    'nop)

  (check-equal?
    (get-axone-signal 'exists 'node)
    'exists)    
)
