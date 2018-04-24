#lang racket

(require "../../lib/load/all.rkt")
(require "../../../settings/APIs.rkt")
(require net/url)
(require json)
(require browser/external)

(define status-output (make-parameter #f))
(define friends-limit (make-parameter #f))

(provide (all-defined-out))

; gets a new access_token if old is expired
; after launching this function it opens the browser, where you should accept the app permissions and then, on the redirected page, check access_token in the GET parameters in the browser address string
(define (authenticate-through-browser)
  (let* ((auth-url "https://oauth.vk.com/authorize")
        (client_id (@. vk/odysseus.id)) ; odysseus app by my main
        (redirect_uri "")
        (display "page")
        (scope #b111111011110111011111)
        (response_type "token")
        (v "5.74")
        (state "odysseus")
        (request-string (url-with-parameters auth-url client_id redirect_uri display scope response_type v state)))
    ; (--- request-string)
    (send-url request-string)))
    ; (json->hash (get-url request-string))))
