#lang racket

; here by some way to describe dependencies on other packages

(define-syntax (package stx)
  #'#t)

(package
  'name "odysseus"
  'version "0.1"
  'dependencies '(("pollen" "1.3")))
