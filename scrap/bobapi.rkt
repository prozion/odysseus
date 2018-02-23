#lang racket

(require net/url)
(require "../lib/load/all.rkt")
(require json)
(require "../../settings/APIs.rkt")

(provide (all-defined-out))

; see documentation: https://bobapi.com/#sec-auth
; curl: https://curl.haxx.se/docs/manpage.html
(define (get-oauth-token (user ""))
  (let* ( (url "https://api.bobapi.com/v1/oauth/token")
          (signature "abc")
          (headers (list
                    "Content-Type" "application/x-www-form-urlencoded"
                    "Authorization" (format "Basic ~a"
                                            (string->base64 (format"~a:~a" user bobapi/consumer-key)))
                    ;"Authorization" (format "OAuth oauth_consumer_key=\"~a\",~n
                    ;                        oauth_signature_method=\"PLAINTEXT\",~n
                    ;                        oauth_signature=\"~a\"" bobapi/consumer-key signature)
                 ))
          (body (url-encode/bytes "grant_type=client_credentials")))
    (post-url url headers body)))

(define (get-bob-data data-type (user "bob"))
  (let*
      ((url (format "https://api.bobapi.com/v1/users/bob/~a?~a"
                      (case data-type
                        (("glucose") "glucose")
                        (else ""))
                      (url-encode "limit=50")))
            (headers (list
                      "Authorization" (format "Bearer ~a"
                                              (string->base64 (format"~a:~a" user bobapi/consumer-key)))
                      ;"Authorization" (format "OAuth oauth_consumer_key=\"~a\",~n
                      ;                        oauth_signature_method=\"PLAINTEXT\",~n
                      ;                        oauth_signature=\"~a\"" bobapi/consumer-key signature)
                   ))
   )
    (get-url url headers)))          
