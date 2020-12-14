#lang racket

(require "hash.rkt")
(require "seqs.rkt")
(require "math.rkt")

(provide emoji/country->flag letter->unicode)

(define LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define base-code 127461)

;(define (letter->unicode-hex letter)
;  (format "&~a;"
;    (dec->hex
;      (+
;        base-code
;        (indexof LETTERS letter)))))

(define (letter->unicode letter)
  (format "&#~a;"
    (+
      base-code
      (indexof LETTERS letter))))

(define (emoji/country->flag country)
  (let ((country (string-upcase country)))
    (str
      (letter->unicode (nth country 1))
      (letter->unicode (nth country 2)))))
