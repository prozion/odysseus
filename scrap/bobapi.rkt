#lang racket

(require net/url)
(require "../lib/all.rkt")
(require json)

(provide (all-defined-out))

; see documentation: https://bobapi.com/#sec-auth
; curl: https://curl.haxx.se/docs/manpage.html
(define (get-current-access-token access-token)
  (let* ( (url "https://api.bobapi.com/v1/oauth/token")
          (client_credentials access-token)
          (user-data null)
          (post-data (@ 'grant_type client_credentials)))
    (post-url url post-data user-data)))
