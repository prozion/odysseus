#lang racket

(require json)
(require net/url)
(require net/uri-codec)
(require "../main.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; read ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-catch (get-page-wiki-content api-url pagename)
  (let* (
        (headers (list "Content-Type: application/x-www-form-urlencoded"))
        (body (format "action=parse&format=json&prop=wikitext&page=~a" pagename)))
    ($ parse.wikitext.*
      (string->jsexpr
        (post-url
          api-url
          headers
          body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; write ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-catch (make-bullet-list alst)
  (string-join
    (map (curry string-append "* ") alst)
    "\n"))

(define session (make-parameter (hash)))

(define (add-to-session h)
  (session (hash-union h (session))))

(define-catch (get-cookies response)
  (let* ((lines (string-split response "\r\n"))
        (cookie-values (filter-map
                        (λ (line)
                          (match line
                            ((pregexp (pregexp "^Set-Cookie: (.+?);.*?$") (list _ cookie-value))
                                cookie-value)
                            (else #f)))
                        lines)))
    ; (---- lines)
    ; (----- 2)
    cookie-values))

(define-catch (make-headers-with-cookies cookies)
  (let* ((headers (list "Content-Type: application/x-www-form-urlencoded"))
        (headers (if cookies
                    (pushr
                      headers
                      (for/fold
                        ((res "Cookie:"))
                        ((cookie cookies))
                        (format "~a ~a;" res cookie)))
                    headers)))
    headers))

(define-catch (wiki-login api-url lgname lgpassword #:token (lgtoken #f) #:cookies (cookies #f))
  (let* (
        (headers (make-headers-with-cookies cookies))
        (body (format "lgname=~a&lgpassword=~a~a" lgname lgpassword (if lgtoken (format "&lgtoken=~a" lgtoken) "")))
        (response (port->string
                    (post-impure-port
                      (string->url (format "~a?action=login&format=json" api-url))
                      (string->bytes/utf-8 body)
                      headers)))
        (parts (string-split response "\r\n\r\n"))
        (body (second parts))
        (body (string->jsexpr body))
        (cookies (get-cookies response)))
    (case ($ login.result body)
      (("NeedToken")
        (add-to-session (hash 'token (uri-encode ($ login.token body))))
        (wiki-login api-url lgname lgpassword #:token ($ token (session)) #:cookies cookies))
      (("Success")
        (add-to-session (hash
                          'cookies cookies
                          'lguserid ($ login.lguserid body)
                          'lgusername ($ login.lgusername body)))
        #t)
      (else
        (errorf "error: ~a" body)))))

(define-catch (get-edittoken api-url)
  (let* (
        (headers (make-headers-with-cookies ($ cookies (session))))
        (response (port->string
                    (get-impure-port
                      (string->url (string-append api-url "?action=query&meta=tokens&format=json"))
                      headers)))
        (parts (string-split response "\r\n\r\n"))
        (body (second parts))
        (body (string->jsexpr body))
        (csrftoken (uri-encode ($ query.tokens.csrftoken body)))
        (_ (add-to-session (hash 'csrftoken csrftoken))))
    csrftoken))

(define-catch (wiki-edit api-url pagetitle content #:summary (summary "") #:bot (bot #t))
  (let* (
        (headers (make-headers-with-cookies ($ cookies (session))))
        (csrftoken (or ($ csrftoken (session)) (get-edittoken api-url)))
        (body (format "title=~a&text=~a&token=~a&summary=~a~a"
                      pagetitle
                      content
                      csrftoken
                      summary
                      (if bot "&bot=1" "&notbot=1")))
        (response (post-url
                      (format "~a?action=edit&format=json" api-url)
                      headers
                      body))
        (response (string->jsexpr response)))
    #t))
