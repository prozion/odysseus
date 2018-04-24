#lang racket

(require net/url)
(require net/base64)
(require "regexp.rkt")
(require "seqs.rkt")
(require (for-syntax "seqs.rkt" "debug.rkt" racket/format))
(require compatibility/defmacro)

(provide (all-defined-out))

(define percent-encoding-table
      '(("\\!"  "#"	  "\\$"	"&"	  "'"	  "\\("	"\\)"	"\\*"	"\\+"	","	  "/"	  ":"	  ";"	  "="	  "\\?"	"@"	  "\\["	"\\]" "%")
        ("%21"	"%23"	"%24"	"%26"	"%27"	"%28"	"%29"	"%2A"	"%2B"	"%2C"	"%2F"	"%3A"	"%3B"	"%3D"	"%3F"	"%40"	"%5B"	"%5D" "%25")))

(define (url-encode astr)
  (re-substitute
    astr
    (first percent-encoding-table)
    (second percent-encoding-table)))

(define (url-encode/bytes astr)
  (string->bytes/utf-8 (url-encode astr)))

(define (url-decode astr)
  (re-substitute
    astr
    (second percent-encoding-table)
    (map
      (λ (x) (exclude-all x "\\"))
      (first percent-encoding-table))))

(define (string->base64 astr)
  (string->bytes/utf-8
    (rtrim
      (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 astr)))
      3)))

;;;;

(define (get-url url (header null))
  (port->string
    (get-pure-port (string->url url) header)))

(define (get-url-bytes url)
  (port->bytes
    (get-pure-port (string->url url))))

; http/1.1 spec: https://tools.ietf.org/html/rfc2616
(define (post-url url headers body)
  (let ((body (if (bytes? body) body (string->bytes/utf-8 body))))
    (port->bytes
      (post-pure-port (string->url url) body headers))))

; (url-with-parameters auth-url client_id redirect_uri display scope response_type v state) ->
; "https://oauth.vk.com/authorize?client_id=client_id&redirect_uri=&display=page&scope=2080223&response_type=token&v=5.74&state=odysseus"
(define-macro (url-with-parameters base-url . args)
  (let* ((pairs (map (λ (x) (list 'cons (~a x) x)) args))
        (pairs (pushl pairs 'list)))
    `(string-append
      ,base-url
      "?"
      (implode
        (map (λ (x) (format "~a=~a" (car x) (cdr x))) ,pairs)
        "&"))))
