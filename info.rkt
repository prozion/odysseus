#lang info

<<<<<<< HEAD
(define version "0.1")
;(define deps '("pollen"))

; other dependencies:
; '(("ImageMagick" "c:/lib/imagemagick" 'ffi)
;   ("magick.rkt" "ffi/magick" 'local-collection))
=======
(define collection "odysseus")
(define deps '("sha" "sxml" "compatibility-lib" "rackunit"))
(define build-deps '("racket-doc" "scribble-lib"))
(define scribblings '(("docs/odysseus.scrbl")))
(define pkg-desc "Library with handy general-purpose functions and macro as well as specific ones. Uses as a midlayer between a bit nurdy vanilla Racket and practical problems met in the everyday life out on the street")
(define version "2021.4")
>>>>>>> odysseus_2022/master
