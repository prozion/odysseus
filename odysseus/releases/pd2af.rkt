#lang racket

(require "../release.rkt")

(extract-files
  (list
    "lib")
  ""
  #:extract-to "c:/denis/denis_core/projects/pd2af/server"
  #:exception-set (list
    ; "sbgn/chemistry"
    ; "sbgn/er"
    ; "sbgn/tests/er"
    "lib/tests"
))

; (extract-files
;   (list
;     "odysseus/system.rkt")
;   ""
;   #:extract-to "c:/denis/denis_core/projects/pd2af/server/extralib"
; )
