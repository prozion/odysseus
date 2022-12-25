#lang racket

(require "main.rkt")

(provide (all-defined-out))

(define (get-command-value cmdargs)
  (let* ((cmdargs-lst (if (vector? cmdargs)
                          (vector->list cmdargs)
                          cmdargs)))
    (if (not-empty? cmdargs-lst)
      (last cmdargs-lst)
      "")))

(define (get-command-options cmdargs)
  (let ((cmdoptions (butlast (vector->list cmdargs))))
      (if (even? (length cmdoptions))
        (apply hash cmdoptions)
        (hash))))
