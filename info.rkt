#lang info

(define collection "odysseus")
(define deps '("compatibility-lib" "rackunit"))
(define build-deps '("racket-doc" "scribble-lib"))
(define scribblings '(("doc/odysseus.scrbl")))
(define pkg-desc "Library with handy general-purpose functions and macro as well as specific ones. Uses as a midlayer between a bit nurdy vanilla Racket and practical problems met in the everyday life out on the street")
(define version "2023.11")
