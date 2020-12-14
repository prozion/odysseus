#lang racket

(require 2htdp/image)
(require "hash.rkt")
(require "debug.rkt")
(require "type.rkt")

(provide (all-defined-out))

(define (get-image-geometry-by-url url-str)
  (hash
    'width (image-width (bitmap/url url-str))))

(define (get-image-geometry path)
  (let ((path (if (path? path) path (string->path path))))
    (hash
      'width (image-width (bitmap/file path)))))
