#lang racket

(require net/url)
(require json)
(require "../../lib/all.rkt")
(require (file "c:/denis/denis_core/settings/bots.rkt"))

(provide (all-defined-out))

; (groups-intersect "https://vk.com/nasevere" "https://vk.com/nikel_ni") -> '(123456 987654 ... )
(define (groups-intersect . groups)
  null)
