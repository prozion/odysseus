#lang info

(define collection "odysseus")
(define deps '("base"
               "htdp-lib"
               "sha" "sxml" "compatibility-lib" "rackunit"))
(define build-deps '("racket-doc" "scribble-lib"))
(define scribblings '(("docs/odysseus.scrbl")))
(define pkg-desc "Library with handy general-purpose functions and macro as well as specific ones. Uses as a midlayer between a bit nurdy vanilla Racket and practical problems met in the everyday life out on the street")
(define version "2021.4")
